{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Numeric.ActiveSet where

import qualified Numeric.LinearAlgebra as LA
import Numeric.LinearAlgebra ((|||), (===), (#>), (<#))
import qualified Data.Set as S
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Vector.Storable as VS
import qualified Control.Foldl as FL
import qualified Data.List as L
import qualified Control.Error as X
--import Debug.Trace (trace)

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

data ActiveSetConfiguration m =
  ActiveSetConfiguration {
  asSolver :: EqualityConstrainedSolver
  , asEpsilon :: Double
  , asMaxIters :: Int
  , log :: Text -> m ()
  }

defaultActiveSetConfig :: (Text -> m ()) -> ActiveSetConfiguration m
defaultActiveSetConfig log' = ActiveSetConfiguration SolveLS 1e-15 1000 log'

{-
findOptimalLH :: ActiveSetConfiguration
              -> LA.Matrix Double
              -> LA.Vector Double
              -> EqualityConstraints
              -> InequalityConstraints
              -> LA.Vector Double
              -> Either Text (LA.Vector Double)
findOptimalLH = findOptimal (StepType lhInitial lhActiveSetStep)
-}

{-
lhActiveSetStep :: NNLSStepFunction WorkingSet
lhActiveSetStep config a b (WorkingData iter x ws) =
  if isFeasible (asEpsilon config) ec ic x'
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
-}

data AS_State = ASFree | ASZero deriving (Show, Eq, Ord)
data LH_WorkingSet = LH_WorkingSet (IM.IntMap AS_State) deriving stock (Show)

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

data LH_NNLSWorkingData = LH_NNLSWorkingData (LA.Vector Double) LH_WorkingSet

data NNLSStep a = NNLS_Optimal Int (LA.Vector Double)
                | NNLS_Error Int Text
                | NNLS_Continue a

data NNLS_LHContinue = LH_NewFeasible Int LH_NNLSWorkingData
                     | LH_TestW Int (LA.Vector Double) LH_NNLSWorkingData
                     | LH_NewInfeasible Int (LA.Vector Double) LH_NNLSWorkingData
                     | LH_UnconstrainedSolve Int (Maybe (LA.Vector Double, Int)) LH_NNLSWorkingData

nnlsX :: NNLS_LHContinue -> LA.Vector Double
nnlsX (LH_NewFeasible _ (LH_NNLSWorkingData x _)) = x
nnlsX (LH_TestW _ _ (LH_NNLSWorkingData x _)) = x
nnlsX (LH_NewInfeasible _ _ (LH_NNLSWorkingData x _)) = x
nnlsX (LH_UnconstrainedSolve _ _ (LH_NNLSWorkingData x _)) = x

nnlsIter :: NNLS_LHContinue -> Int
nnlsIter (LH_NewFeasible n _) = n
nnlsIter (LH_TestW n _ _) = n
nnlsIter (LH_NewInfeasible n _ _) = n
nnlsIter (LH_UnconstrainedSolve n _ _) = n


lhNNLSStep :: Monad m => ActiveSetConfiguration m -> LA.Matrix Double -> LA.Vector Double -> NNLS_LHContinue -> m (NNLSStep NNLS_LHContinue)
lhNNLSStep config a b lhc = case lhc of
  LH_NewFeasible n wd@(LH_NNLSWorkingData x ws) -> do
    log config $ "(n=" <> show n <> ") Feasible x=" <> show x <> "\n" <> " WorkingSet=" <> show ws
    let w = LA.tr a LA.#> (b - a LA.#> x)
    pure $ NNLS_Continue (LH_TestW n w  wd)
  LH_TestW n w (LH_NNLSWorkingData x ws) -> do
    log config $ "(n=" <> show n <> ") TestW x=" <> show x <> "\n" <> " WorkingSet=" <> show ws
    log config $ "w=" <> show w
    let (freeIS, zeroIS) = workingIS ws
        emptyZ = IS.size zeroIS == 0
        wAtZeros = subVector zeroIS w
        allNegW =  Nothing == VS.find (> 0) wAtZeros
    if (emptyZ || allNegW)
      then pure $ NNLS_Optimal n x
      else case vecIndexLargest (addZeroesV freeIS wAtZeros) of -- this only works because we know largest is > 0
             Nothing -> pure $ NNLS_Error n "lhNNLSStep: vecIndexLargest returned Nothing. Empty w?"
             Just (maxWIndex, _) -> do
               let newWorkingSet = toState ASFree [maxWIndex] ws
               pure $ NNLS_Continue $ LH_UnconstrainedSolve n (Just (w, maxWIndex)) $ LH_NNLSWorkingData x newWorkingSet
  LH_UnconstrainedSolve n mW wd@(LH_NNLSWorkingData x ws) -> do
    log config $ "(n=" <> show n <> ") Unconstrained Solve x=" <> show x <> "\n" <> " WorkingSet=" <> show ws
    log config $ "mW=" <> show mW
    let (freeIS, zeroIS) = workingIS ws
        newA = subMatrix freeIS a
    log config $ "newA=" <> show newA
    let lsSolution = case (asSolver config) of
          SolveLS -> LA.flatten $ LA.linearSolveLS newA (LA.asColumn b)
          SolveSVD -> LA.flatten $ LA.linearSolveSVD newA (LA.asColumn b)
        g (w, t) = if (addZeroesV zeroIS lsSolution) VS.! t < 0 then Just (w ,t) else Nothing
    case mW >>= g of
      Just (w, t) -> do
        log config $ "solution is < 0 at index of max W. Zeroing W there and trying again."
        pure $ NNLS_Continue $ LH_TestW (n + 1) (w VS.// [(t, 0)]) wd
      Nothing ->  case vecIndexSmallest lsSolution of
                    Nothing -> pure $ NNLS_Error n "lhNNLSStep: vecIndexSmallest returned Nothing. Empty lsSolution?"
                    Just (_, smallest) -> if smallest > 0
                      then pure $ NNLS_Continue $ LH_NewFeasible (n + 1) (LH_NNLSWorkingData (addZeroesV zeroIS lsSolution) ws)
                      else pure $ NNLS_Continue $ LH_NewInfeasible (n + 1) lsSolution (LH_NNLSWorkingData x ws)
  LH_NewInfeasible n z (LH_NNLSWorkingData x ws) -> do
    log config $ "(n=" <> show n <> ") Infeasible z=" <> show z <> "\n" <> " WorkingSet=" <> show ws <> "\nx=" <> show x
    let (freeIS, zeroIS) = workingIS ws
        z' = addZeroesV zeroIS z
        x' = subVector freeIS x
        y = VS.zipWith (\xx zz -> xx / (xx - zz)) x' z'
    log config $ "alphas=" <> show y
    let s1 = zip3 (VS.toList y) (VS.toList z') [0..]
    log config $ "s1=" <> show s1
    let g (a, _, _) = a
        s2 = sortOn g $ filter (\(_, q, _) -> q <= 0) s1
        mAlpha = g <$> viaNonEmpty head s2
    case mAlpha of
      Nothing -> pure $ NNLS_Error n "lhNNLSStep:Empty list in alpha finding step in LH_NewInFeasible"
      Just alpha -> do
        log config $ "alpha=" <> show alpha
        let x' = x + VS.map (* alpha) (x - z)
            newZeroIs = fmap snd $ filter ((< asEpsilon config) . abs . fst) $ zip (VS.toList x') [0..]
            ws' = toState ASZero newZeroIs ws
        pure $ NNLS_Continue $ LH_UnconstrainedSolve n Nothing (LH_NNLSWorkingData x' ws')

--lift :: Monad m => m a -> X.ExceptT m a
--lift = X.ExceptT . fmap Right

optimalNNLS :: forall m . Monad m
            => ActiveSetConfiguration m
            -> LA.Matrix Double
            -> LA.Vector Double
            -> m (Either Text (Int, LA.Vector Double))
optimalNNLS config a b  = X.runExceptT $ do
  _ <- case checkDimensionsNNLS a b (VS.replicate (LA.cols a) 0) of
    Left msg -> X.throwE msg
    Right _ -> pure ()
  lift $ log config $ "a=" <> show a
  lift $ log config $ "b=" <> show b
  let go :: NNLSStep NNLS_LHContinue -> X.ExceptT Text m (Int, LA.Vector Double)
      go (NNLS_Optimal n x') = pure (n, x')
      go (NNLS_Error iter msg) = X.throwE $ "ActiveSet.findOptimal (after " <> show iter <> " steps): " <> msg
      go (NNLS_Continue c) = if nnlsIter c < asMaxIters config
                             then lift (lhNNLSStep @m config a b c) >>= go
                             else X.throwE $ "ActiveSet.optimalNNLS maxIters (" <> show (nnlsIter c) <> ") reached: x=" <> show (nnlsX c)
      xSize = LA.cols a
      initialX = VS.replicate xSize 0
      initialWorkingSet = LH_WorkingSet $ IM.fromList $ zip [0..(xSize - 1)] (L.replicate xSize ASZero)
  go $ NNLS_Continue (LH_NewFeasible 0 (LH_NNLSWorkingData initialX initialWorkingSet))
  -- trace ("initial working set is" <> show initialWorkingSet) $ go 0 $ NotOptimal initialWorkingSet x

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
data EqualityConstrainedSolver = SolveLS | SolveSVD

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
