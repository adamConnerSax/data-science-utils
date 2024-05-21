{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
module Numeric.ActiveSet
  (
  module Numeric.ActiveSet
  , module Numeric.NNLS.Types
  ) where


import Numeric.NNLS.Types
import qualified Numeric.LinearAlgebra as LA
import Numeric.LinearAlgebra ((|||), (===), (#>), (<#))
import qualified Data.Set as S
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Vector.Storable as VS
import qualified Control.Foldl as FL
import qualified Control.Lens as Lens
import Control.Lens (view, views, use, uses, set, assign, over, (%=))
import qualified Data.List as L

import qualified Control.Monad.RWS.Strict as RWS
import qualified Control.Error as X
import Debug.Trace (trace)

-- after Chen & Ye: https://arxiv.org/pdf/1101.6081
projectToSimplex :: VS.Vector Double -> VS.Vector Double
projectToSimplex y = VS.fromList $ fmap (\x -> max 0 (x - tHat)) yL
  where
    yL = VS.toList y
    n = VS.length y
    sY = sort yL
    t i = (FL.fold FL.sum (L.drop i sY) - 1) / realToFrac (n - i)
    tHat = go (n - 1)
    go 0 = t 0
    go k = let tk = t k in if tk > sY L.!! k then tk else go (k - 1)

initialLHWorkingSet:: Int -> LH_WorkingSet
initialLHWorkingSet n = LH_WorkingSet $ IM.fromList $ zip [0..(n-1)] (replicate n ASZero)

toState :: AS_State -> [Int] -> LH_WorkingSet -> LH_WorkingSet
toState s is (LH_WorkingSet im) =  LH_WorkingSet $ IM.fromList (zip is (replicate (length is) s)) <> im

workingIS :: LH_WorkingSet -> (IS.IntSet, IS.IntSet)
workingIS (LH_WorkingSet im) = let (fim, zim) = IM.partition (== ASFree) im in (IM.keysSet fim, IM.keysSet zim)

-- keep only columns in the index set
subMatrix :: IS.IntSet -> LA.Matrix Double -> LA.Matrix Double
subMatrix is m =  m LA.?? (LA.All, LA.Pos (LA.idxs $ IS.toList is))

-- keep only elts in the index set
subVector :: IS.IntSet -> LA.Vector Double -> LA.Vector Double
subVector is v = VS.ifilter (\n _ -> IS.member n is) v

-- add zeroes at the indices given
addZeroesV :: IS.IntSet -> LA.Vector Double -> LA.Vector Double
addZeroesV is v = VS.fromList $ IS.foldl' f (VS.toList v) is where
  f l i = let (h, t) = L.splitAt i l in h <> (0 : t)

modifyAlgoData :: Monad m => (a -> a) -> ASM a m ()
modifyAlgoData f = algoData %= f  where

nnlsX :: Monad m => (a -> LA.Vector Double) -> ASM a m (LA.Vector Double)
nnlsX xFromAlgoData = xFromAlgoData <$> use algoData

getAlgoData :: Monad m => ASM a m a
getAlgoData = use algoData

getIters :: Monad m => ASM a m Int
getIters = use algoIters

-- returns old iters
nnlsIterPlusOne :: Monad m => ASM a m ()
nnlsIterPlusOne = algoIters %= (+1) >> pure ()

logASM :: Monad m => LogF m -> Text -> ASM a m ()
logASM lf t = ASM $ lift $ lift $ lf t

logStepLH_NNLS :: Monad m => LogF m -> Text ->  ASMLH m ()
logStepLH_NNLS lf t = do
  iters <- getIters
  LH_NNLSWorkingData x ws <- getAlgoData
  (freeIS, zeroIS) <- indexSets
  getIters >>= \n -> logASM lf $ t <> " (n=" <> show n <> "): "
                     <> "; x=" <> show x
                     <> "; freeI=" <> show (IS.toList freeIS) <> "; zeroI=" <> show (IS.toList zeroIS)

indexSets :: Monad m => ASMLH m (IS.IntSet, IS.IntSet)
indexSets = uses (algoData . lhWS) workingIS

lhNNLSStep :: Monad m => LogF m -> LA.Matrix Double -> LA.Vector Double -> NNLS_LHContinue -> ASMLH m (NNLSStep NNLS_LHContinue)
lhNNLSStep logF a b lhc = do
  let log = logASM logF . (" " <>)
      logStep = logStepLH_NNLS logF
      updateX = assign (algoData . lhX)
      getX = use (algoData. lhX)
      getWS = use (algoData. lhWS)
  case lhc of
    LH_NewFeasible -> do
      logStep "Feasible"
      let w x = LA.tr a LA.#> (b - a LA.#> x)
      use (algoData . lhX) >>= pure . NNLS_Continue . LH_TestW . w
    LH_TestW w  -> do
      logStep "TestW"
      log $ "w=" <> show w
      (freeIS, zeroIS) <- indexSets
      let emptyZ = IS.size zeroIS == 0
          wAtZeros = subVector zeroIS w
          allNegW =  Nothing == VS.find (> 0) wAtZeros
      if (emptyZ || allNegW)
        then getX >>= pure . NNLS_Optimal
        else case vecIndexLargest (addZeroesV freeIS wAtZeros) of -- this only works because we know largest is > 0
               Nothing -> pure $ NNLS_Error "lhNNLSStep: vecIndexLargest returned Nothing. Empty w?"
               Just (maxWIndex, _) -> do
                 algoData . lhWS %= toState ASFree [maxWIndex]
                 pure $ NNLS_Continue $ LH_UnconstrainedSolve (Just (w, maxWIndex))
    LH_UnconstrainedSolve mW  -> do
      logStep "UnconstrainedSolve"
      log $ "mW=" <> show mW
      (freeIS, zeroIS) <- indexSets
      let newA = subMatrix freeIS a
      log $ "newA=" <> show newA
      solver <- RWS.asks asSolver
      let lsSolution = case solver of
            SolveLS -> LA.flatten $ LA.linearSolveLS newA (LA.asColumn b)
            SolveSVD -> LA.flatten $ LA.linearSolveSVD newA (LA.asColumn b)
          g (w, t) = if (addZeroesV zeroIS lsSolution) VS.! t < 0 then Just (w ,t) else Nothing
      nnlsIterPlusOne
      case mW >>= g of
        Just (w, t) -> do
          log $ "solution=" <> show (addZeroesV zeroIS lsSolution) <> " is < 0 at index of max W. Zeroing W there and trying again."
          pure $ NNLS_Continue $ LH_TestW (w VS.// [(t, 0)])
        Nothing ->  case vecIndexSmallest lsSolution of
                      Nothing -> pure $ NNLS_Error "lhNNLSStep: vecIndexSmallest returned Nothing. Empty lsSolution?"
                      Just (_, smallest) -> if smallest > 0
                        then updateX (addZeroesV zeroIS lsSolution) >> pure (NNLS_Continue LH_NewFeasible)
                        else pure $ NNLS_Continue $ LH_NewInfeasible lsSolution
    LH_NewInfeasible z -> do
      logStep "Infeasible"
      log $ "z=" <> show z
      (freeIS, zeroIS) <- indexSets
      x <- getX
      LH_WorkingSet ws <- getWS
      let z' = addZeroesV zeroIS z
          y = VS.zipWith (\xx zz -> xx / (xx - zz)) x z'
      let s1 = zip3 (VS.toList y) (VS.toList z') (IM.elems ws)
      log $ "[(alpha, z, varState)]=" <> show s1
      let g (a, _, _) = a
          s2 = sortOn g $ filter (\(_, q, varState) -> q <= 0 && varState == ASFree) s1
          mAlpha = g <$> viaNonEmpty head s2
      case mAlpha of
        Nothing -> pure $ NNLS_Error "lhNNLSStep:Empty list in alpha finding step in LH_NewInFeasible"
        Just alpha -> do
          log $ "alpha=" <> show alpha
          eps <- RWS.asks asEpsilon
          let x' = x + VS.map (* alpha) (x - z)
              newZeroIs = fmap snd $ filter ((< eps) . abs . fst) $ zip (VS.toList x') [0..]
          updateX x'
          algoData . lhWS %= toState ASFree newZeroIs
          pure $ NNLS_Continue $ LH_UnconstrainedSolve Nothing


optimalNNLS :: forall m . Monad m
            => LogF m
            -> ActiveSetConfiguration
            -> LA.Matrix Double
            -> LA.Vector Double
            -> m (Either Text (LA.Vector Double), Int)
optimalNNLS logF config a b  = do
  let xSize = LA.cols a
      initialX = VS.replicate xSize 0
      initialWorkingSet = LH_WorkingSet $ IM.fromList $ zip [0..(xSize - 1)] (L.replicate xSize ASZero)

  runASM logF config (LH_NNLSWorkingData initialX initialWorkingSet) $ \lf -> do
    _ <- case checkDimensionsNNLS a b (VS.replicate (LA.cols a) 0) of
      Left msg -> throwASM msg
      Right _ -> pure ()
    logASM lf $ "a=" <> show a
    logASM lf $ "b=" <> show b
    let go :: NNLSStep NNLS_LHContinue -> ASMLH m (LA.Vector Double)
        go (NNLS_Optimal x) = pure x
        go (NNLS_Error msg) = getIters >>= \n -> throwASM $ "ActiveSet.findOptimal (after " <> show n <> " steps): " <> msg
        go (NNLS_Continue c) = getIters >>= \n -> if n < asMaxIters config
                                                  then lhNNLSStep logF a b c >>= go
                                                  else use (algoData . lhX)
                                                       >>= \x -> throwASM $ "ActiveSet.optimalNNLS maxIters (" <> show n <> ") reached: x=" <> show x

    go $ NNLS_Continue LH_NewFeasible
  -- trace ("initial working set is" <> show initialWorkingSet) $ go 0 $ NotOptimal initialWorkingSet x

optimalLDP :: forall m . Monad m
           => LogF m
           -> ActiveSetConfiguration
           -> InequalityConstraints
           -> m (Either Text (LA.Vector Double), Int)
optimalLDP logF config ic' = do
  let (IC g h) = convertInequalityConstraints ic'
      n = LA.rows e
      e = LA.tr g === LA.asRow h
      f = VS.fromList (L.replicate n 0 <> [1])
  (eRes, n) <- optimalNNLS logF config e f
  case eRes of
    Left msg -> pure (Left ("optimalLDP: error in optimalNNLS step" <> msg), n)
    Right r -> do
      if LA.norm_2 r < asEpsilon config
        then pure (Left "optimalLDP: incompatible inequalities!", n)
        else (let (x', vn) = VS.splitAt (n  - 1) r
                  r_n = vn VS.! 0
                  x = VS.map (\y -> negate y / r_n) x' -- x_k = -r_k/r_{n+1}
              in pure (Right x, n)
             )

data WorkingData a = WorkingData { iters:: !Int, curX :: !(LA.Vector Double), stepData :: !a} deriving stock (Show)
-- a carries algorithm dependent extra data for next step
data StepResult a = OptimalFound Int (LA.Vector Double) | NotOptimal (WorkingData a) | StepError Int Text deriving stock (Show)

{-
data NNLSStepType a = StepType { stepInitial :: NNLSInitialNonOptimal a, stepFunction :: NNLSStepFunction a}


type NNLSInitialNonOptimal a m =  (ActiveSetConfiguration m
                                  -> LA.Matrix Double
                                  -> LA.Vector Double
                                  -> LA.Vector Double
                                  -> StepResult a
                                  )

type NNLSStepFunction a m = (ActiveSetConfiguration m
                            -> LA.Matrix Double
                            -> LA.Vector Double
                            -> WorkingData a
                            -> StepResult a
                            )
-}
data InequalityConstraints where
  SimpleBounds :: LA.Vector Double -> LA.Vector Double -> InequalityConstraints
  -- ^ l <= x <= u
  MatrixUpper :: LA.Matrix Double -> LA.Vector Double -> InequalityConstraints
  -- ^ Ax <= b
  MatrixLower :: LA.Matrix Double -> LA.Vector Double -> InequalityConstraints
  -- ^ Ax >= b

emptyInequalityConstraints :: Int -> InequalityConstraints
emptyInequalityConstraints n = MatrixLower (LA.matrix n []) (VS.fromList [])

data EqualityConstraints = EqualityConstraints !(LA.Matrix Double) !(LA.Vector Double) deriving stock (Show)

emptyEqualityConstraints :: Int -> EqualityConstraints
emptyEqualityConstraints n = EqualityConstraints (LA.matrix n []) (VS.fromList [])

data IC = IC (LA.Matrix Double) (LA.Vector Double) deriving stock (Show)

convertInequalityConstraints :: InequalityConstraints -> IC
convertInequalityConstraints (SimpleBounds l u ) = IC a b where
  a = LA.ident (LA.size l) === negate (LA.ident $ LA.size u)
  b = VS.concat [l, negate u]
convertInequalityConstraints (MatrixUpper a b) = IC (negate a) (negate b)
convertInequalityConstraints (MatrixLower a b) = IC a b

isFeasible :: Double -> EqualityConstraints -> IC -> LA.Vector Double -> Bool
isFeasible eps (EqualityConstraints g h) (IC c d) x =  eV && iV where
  eV = (VS.find ((> eps) . abs) $ (g #> x) - h) == Nothing
  iV = (VS.find ((< negate eps)) $ (c #> x) - d) == Nothing

newtype WorkingSet = WorkingSet { workingSet :: Set Int} deriving stock (Show)

-- given a set of inequality constraints, find active and violated sets for a given vector
getWorkingSet :: Double -> IC -> LA.Vector Double -> WorkingSet
getWorkingSet eps (IC a b) u =  f $ VS.foldl' g (0, mempty, mempty) $ (a #> u) - b
  where
    g (n, a', v') x
      | abs x < eps = (n+ 1, S.insert n a', v') -- active
      | x < -eps = (n + 1, a', S.insert n v') -- violated
      | otherwise = (n + 1, a', v') -- inactive
    f (_, a', _) = WorkingSet a'


addToWorkingSet :: Int -> WorkingSet -> WorkingSet
addToWorkingSet k = WorkingSet . S.insert k . workingSet

removeFromWorkingSet :: Int -> WorkingSet -> WorkingSet
removeFromWorkingSet k = WorkingSet . S.delete k . workingSet

workingSetConstraints :: WorkingSet -> IC -> IC
workingSetConstraints (WorkingSet s) (IC c d) = IC c' d' where
  c' = c LA.?? (LA.Pos (LA.idxs  $ S.toList s), LA.All)
  d' = VS.ifilter (\n _ -> S.member n s) d

mostNegativeLambdaIndex :: WorkingSet -> LA.Vector Double -> Maybe Int
mostNegativeLambdaIndex (WorkingSet s) l = safeIndex smallestWorking (S.toList s) where
  f (n, sIndex, smallest) x = if x < smallest then (n + 1, n, x) else (n + 1, sIndex, smallest)
  (_, smallestWorking, _) = VS.foldl' f (0, 0, 0) l
  safeIndex k l' = if k > length l' - 1 then Nothing else Just (l' L.!! k)

vecIndexLargest :: LA.Vector Double -> Maybe (Int, Double)
vecIndexLargest v = (indexLargest,) <$>  mLargest where
  f (n, sIndex, mLargest) x = case mLargest of
    Nothing -> (n+1, n, Just x)
    Just largest -> if x > largest then (n + 1, n, Just x) else (n + 1, sIndex, Just largest)
  (_, indexLargest, mLargest) = VS.foldl' f (0, 0, Nothing) v

vecIndexSmallest :: LA.Vector Double -> Maybe (Int, Double)
vecIndexSmallest v = (indexSmallest,) <$> mSmallest where
  f (n, sIndex, mSmallest) x = case mSmallest of
    Nothing -> (n +1, n, Just x)
    Just smallest -> if x < smallest then (n + 1, n, Just x) else (n + 1, sIndex, Just smallest)
  (_, indexSmallest, mSmallest) = VS.foldl' f (0, 0, Nothing) v


largestLambdaIndex :: WorkingSet -> LA.Vector Double -> Maybe Int
largestLambdaIndex (WorkingSet s) l = safeIndex largestWorking (S.toList s) where
  f (n, sIndex, largest) x = if x > largest then (n + 1, n, x) else (n + 1, sIndex, largest)
  (_, largestWorking, _) = VS.foldl' f (0, 0, 0) l
  safeIndex k l' = if k > length l' - 1 then Nothing else Just (l' L.!! k)

-- LS will work if (A' C') is right invertible (linearly indep rows) and C is also right invertible
-- Otherwise you might need the SVD


-- solve ||Ax - b||_2 s.t. Cx = d and produce Least-Squares x as well as Lagrange multipliers for constraints.
-- These are the KKT equations
-- via G(x,l) = ||Ax - b||_2 + l'(Cx - d)
-- dG/dx = 0 and dG/dl = 0
-- solving |2A'A C'| |x| = |2b'A|
--         |C    0 | |l|   |d|
-- LHS matrix is non-singular but might be over or under-determined
equalityConstrained :: EqualityConstrainedSolver
                    -> LA.Matrix Double
                    -> LA.Vector Double
                    -> LA.Matrix Double
                    -> LA.Vector Double
                    -> (LA.Vector Double, LA.Vector Double)
equalityConstrained solver a b c d = VS.splitAt n $ LA.flatten s  where
  n = LA.cols a
  cRows = LA.rows c
  q = (2 * (LA.tr a) <> a ||| LA.tr c) === (c ||| LA.konst 0 (cRows, cRows))
  y = VS.concat [2 * b <# a, d]
  s = case solver of
    SolveLS -> LA.linearSolveLS q (LA.asColumn y)
    SolveSVD -> LA.linearSolveSVD q (LA.asColumn y)


-- for a given set of inequality constraints, a feasible vector x and a perturbation p, find the maximum r
-- s.t., x + r p is feasible as well as the index which limited it
-- we do this by finding the max r for each element and then choosing the smallest of those
maxAlpha :: IC -> LA.Vector Double -> LA.Vector Double -> Maybe (Double, Int)
maxAlpha (IC a b) x p = result where
  rs = VS.zipWith3 (\ax ap b' -> if ax > b'
                                 then
                                   if ap < 0
                                   then (b' - ax) / ap
                                   else 1
                                 else -1) (a #> x) (a #> p) b
  result = fmap (\(a', b') -> (b', a')) $ viaNonEmpty head $ L.sortOn snd $ filter ((> 0) . snd) $ zip [(0 :: Int)..] $ VS.toList rs


checkDimensions :: LA.Matrix Double
                -> LA.Vector Double
                -> EqualityConstraints
                -> IC
                -> LA.Vector Double
                -> Either Text ()
checkDimensions a b (EqualityConstraints g h) (IC c d) x = do
  let (aRows, aCols) = LA.size a
      bRows = LA.size b
  _ <- if bRows /= aRows
       then Left ("target vector b is different length (" <> show bRows <> ") than A has rows (" <> show aRows <> ")")
       else Right ()
  let (gRows, gCols) = LA.size g
      hRows = LA.size h
  _ <- if gCols /= aCols
       then Left ("Equality constraint matrix G has different number of cols (" <> show gCols <> ") than A has cols (" <> show aCols <> ")")
       else Right ()
  _ <- if gRows /= hRows
       then Left ("Equality constraint matrix G has different number of rows (" <> show gRows <> ") than constraint RHS has cols (" <> show hRows <> ")")
       else Right ()
  let (cRows, cCols) = LA.size c
      dRows = LA.size d
  _ <- if cCols /= aCols
       then Left ("Inequality constraint matrix C has different number of cols (" <> show cCols <> ") than A has cols (" <> show aCols <> ")")
       else Right ()
  _ <- if cRows /= dRows
       then Left ("Inequality constraint matrix C has different number of rows (" <> show cRows <> ") than constraint RHS has cols (" <> show dRows <> ")")
       else Right ()
  let xRows = LA.size x
  if xRows /= aCols
    then Left ("guess vector x is different length (" <> show xRows <> ") than A has cols (" <> show aCols <> ")")
    else Right ()


checkDimensionsNNLS :: LA.Matrix Double
                    -> LA.Vector Double
                    -> LA.Vector Double
                    -> Either Text ()
checkDimensionsNNLS a b x = do
  let (aRows, aCols) = LA.size a
      bRows = LA.size b
  _ <- if bRows /= aRows
       then Left ("target vector b is different length (" <> show bRows <> ") than A has rows (" <> show aRows <> ")")
       else Right ()
  let xRows = LA.size x
  if xRows /= aCols
    then Left ("guess vector x is different length (" <> show xRows <> ") than A has cols (" <> show aCols <> ")")
    else Right ()


{-
lhActiveSetStep :: NNLSStepFunction WorkingSet
lhActiveSetStep config a b ec@(EqualityConstraints g h) ic (WorkingData iter x ws) =
  let  (IC c' d') = workingSetConstraints ws ic
       fullC = g === c'
       fullD = VS.concat [h, d']
       numEC = LA.rows g
       (x', allL) = equalityConstrained (asSolver config) a b fullC fullD
  in if isFeasible (asEpsilon config) ec ic x'
     then if Nothing == VS.find (> 0) (VS.drop numEC allL)
          then OptimalFound iter x'
          else case largestLambdaIndex ws (VS.drop numEC allL) of
                 Nothing -> StepError iter "Some lambda is positive but there was an error finding the index of it."
                 Just n -> {- trace ("Removing " <> show n <> " from working set and x'=" <> show x') $-}
                   NotOptimal
                   $ WorkingData (iter + 1) x' (removeFromWorkingSet n ws)
     else
       let u = x' - x
       in case maxAlpha ic x u of
            Nothing -> StepError iter "New x is not feasible but there was an error finding alpha such that x + alpha u is."
            Just (alpha, n) ->  let x'' = (x + VS.map (* alpha) u)
                                in {- trace ("Adding " <> show n <> " to working set and x'=" <> show x'') $ -}
                                  NotOptimal $ WorkingData (iter + 1) x'' (addToWorkingSet n ws)


findOptimal :: StepType a
            -> ActiveSetConfiguration
            -> LA.Matrix Double
            -> LA.Vector Double
            -> EqualityConstraints
            -> InequalityConstraints
            -> LA.Vector Double
            -> Either Text (LA.Vector Double)
findOptimal step config a b ec@(EqualityConstraints g h) ic' x = do
  let ic@(IC c d) = convertInequalityConstraints ic'
  _<- checkDimensions a b ec ic x
  _ <- if isFeasible (asEpsilon config) ec ic x then Right ()
       else Left
            $ "ActiveSet.findOptimal: Given guess is not a feasible solution!"
            <> "Gx - h = " <> show ((g #> x) - h)
            <> "Cx - d = " <> show ((c #> x) - d)
  let go (OptimalFound _ x') = Right x'
      go (StepError iter msg) = Left $ "ActiveSet.findOptimal (after " <> show iter <> " steps): " <> msg
      go (NotOptimal wd) = if iters wd < asMaxIters config
                           then go $ (stepFunction step) config a b ec ic wd
                           else Left $ "ActiveSet.findOptimal: maxIters (" <> show (iters wd) <> ") reached: x=" <> show (curX wd)
  go $ (stepInitial step) config a b ec ic x
  -- trace ("initial working set is" <> show initialWorkingSet) $ go 0 $ NotOptimal initialWorkingSet x

-}
