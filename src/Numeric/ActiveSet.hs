{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}
module Numeric.ActiveSet
  (
  module Numeric.ActiveSet
  , module Numeric.NNLS.Types
  ) where


import Numeric.NNLS.Types
import Numeric.NNLS.IncrementalQR as IQR
import qualified Numeric.LinearAlgebra as LA
import Numeric.LinearAlgebra ((|||), (===), (#>), (<#))
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Sequence as Seq
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import qualified Control.Foldl as FL
import qualified Control.Lens as Lens
import Control.Lens (view, views, use, uses, set, assign, over, (%=))
import qualified Data.List as L

import qualified Control.Monad.RWS.Strict as RWS
import qualified Control.Monad.Except as RWS
import qualified Control.Error as X
import Data.Maybe (fromJust)
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

initialNNLSWorkingDataLH :: Int -> LH_NNLSWorkingData
initialNNLSWorkingDataLH n = LH_NNLSWorkingData initialX initialWorkingSet Nothing Nothing where
  initialX = VS.replicate n 0
  initialWorkingSet = LH_WorkingSet $ IM.fromList $ zip [0..(n - 1)] (L.replicate n ASZero)

optimalNNLS :: forall m . Monad m
            => LogF m
            -> ActiveSetConfiguration
            -> LA.Matrix Double
            -> LA.Vector Double
            -> m (Either Text (LA.Vector Double), Int)
optimalNNLS logF config a b  = do
  let xSize = LA.cols a
  runASM logF config (initialNNLSWorkingDataLH xSize) $ nnlsAlgo a b

nnlsAlgo ::  forall m . Monad m
         => LA.Matrix Double
         -> LA.Vector Double
         -> LogF m
         -> ASMLH m (LA.Vector Double)
nnlsAlgo a b lf = do
  nnlsCheckDims a b
  logASM lf $ "solving NNLS"
--  logASM lf $ "a=" <> show a
--  logASM lf $ "b=" <> show b
  let xSize = LA.cols a
      go :: NNLSStep NNLS_LHContinue -> ASMLH m (LA.Vector Double)
      go (NNLS_Optimal x) = do
        ASM $ X.hoistEither $ checkConstraints "(NNLS)" 1e-8 (MatrixLower (LA.ident xSize) $ VS.replicate xSize 0) x
        logASM lf ("NNLS solution: x =" <> show x)
        pure x
      go (NNLS_Error msg) = getIters >>= \n -> throwASM $ "ActiveSet.findOptimal (after " <> show n <> " steps): " <> msg
      go (NNLS_Continue c) = do
        iters' <- getIters
        maxIters <- RWS.asks cfgMaxIters
        if iters' < maxIters
          then lhNNLSStep lf a b c >>= go
          else use (algoData . lhX)
               >>= \x -> throwASM $ "ActiveSet.optimalNNLS maxIters (" <> show iters' <> ") reached: x=" <> show x

  go $ NNLS_Continue LH_Setup

nnlsCheckDims :: Monad m
              => LA.Matrix Double
              -> LA.Vector Double
              -> ASMLH m ()
nnlsCheckDims a b = do
  let (aRows, aCols) = LA.size a
      bLength = LA.size b
  xLength <- LA.size <$> use (algoData . lhX)
  checkPair "NNLS" (aRows, "rows(A)") (bLength, "length(b)")
  checkPair "NNLS" (aCols, "cols(A)") (xLength, "length(x)")
  fullRank "a" a
--  fullColumnRank "a" a

optimalLDP :: forall m . Monad m
           => LogF m
           -> ActiveSetConfiguration
           -> InequalityConstraints
           -> m (Either Text (LA.Vector Double), Int)
optimalLDP logF config ic' = do
  let (IC g _) = convertInequalityConstraints ic'
      nnlsSize = LA.rows g
  runASM logF config (initialNNLSWorkingDataLH nnlsSize) $ ldpAlgo ic'

ldpAlgo :: forall m . Monad m
        => InequalityConstraints
        -> LogF m
        -> ASMLH m (LA.Vector Double)
ldpAlgo ic lf = do
  let (IC g h) = convertInequalityConstraints ic
  ldpCheckDims (IC g h)
  let n = LA.cols g
      e = LA.tr g === LA.asRow h
      f = VS.fromList (L.replicate n 0 <> [1])
  logASM lf $ "Solving LDP" --"with G=" <> show g <> "\n h=" <> show h
  fullRank "E" e
  u <- nnlsAlgo e f lf
  epsilon <- RWS.asks cfgEpsilon
  let r = (e #> u) - f
  if LA.norm_2 r < epsilon
    then throwASM "optimalLDP: incompatible inequalities!"
    else (do
             let (x', vnp1) = VS.splitAt n r
                 r_np1 = vnp1 VS.! 0
                 x = VS.map (\y -> negate y / r_np1) x' -- x_k = -r_k/r_{n+1}
             ASM $ X.hoistEither $ checkConstraints "(LDP)" 1e-8 ic x
             logASM lf $ "LDP solution: x =" <> show x
             pure x
             )

ldpCheckDims :: Monad m => IC -> ASMLH m ()
ldpCheckDims (IC g h) = do
  let gRows = LA.rows g
      hLength = LA.size h
  checkPair "LDP" (gRows, "rows(G)") (hLength, "length(h)")


optimalLSI :: forall m . Monad m
           => LogF m
           -> ActiveSetConfiguration
           -> LSI_E
           -> LA.Vector Double
           -> InequalityConstraints
           -> m (Either Text (LA.Vector Double), Int)
optimalLSI logF config lsiE f ic = do
  let nnlsSize = LA.rows (originalE lsiE)
  runASM logF config (initialNNLSWorkingDataLH nnlsSize) $ lsiAlgo lsiE f ic

lsiAlgo :: forall m . Monad m
        => LSI_E
        -> LA.Vector Double
        -> InequalityConstraints
        -> LogF m
        -> ASMLH m (LA.Vector Double)
lsiAlgo lsiE f ic lf = do
  logASM lf $ "Solving LSI" -- with E=" <> show (originalE lsiE) <> "\n f=" <> show f <> "\nConstraints=" <> show ic
  lsiCheckDimsAndRank lsiE f ic
  config <- RWS.ask
  (icz, zTox) <- ASM $ RWS.mapExceptT id $ lsiICAndZtoX config lsiE f ic
  z <- ldpAlgo icz lf
  let x = zTox z
  ASM $ X.hoistEither $ checkConstraints "(LSI)" 1e-8 ic x
  logASM lf ("LSI solution: x =" <> show x)
  pure x

lsiCheckDimsAndRank :: Monad m => LSI_E -> LA.Vector Double -> InequalityConstraints -> ASMLH m ()
lsiCheckDimsAndRank lsiE f ic = do
  let (IC g h) = convertInequalityConstraints ic
      e = originalE lsiE
      (eRows, eCols) = LA.size e
      (gRows, gCols) = LA.size g
      hLength = LA.size h
      fLength = LA.size f
  checkPair "LSI" (eRows, "rows(E)") (fLength, "length(f)")
  checkPair "LSI" (gRows, "rows(G)") (hLength, "length(h)")
  checkPair "LSI" (eCols, "cols(E)") (gCols, "cols(G)")
  fullColumnRank "E" e

checkPair :: Monad m => Text -> (Int, Text) -> (Int, Text) ->  ASMLH m ()
checkPair t (n1, t1) (n2, t2) =
  when (n1 /= n2)
  $ throwASM
  $ "Inconsistent dimensions in " <> t <> ": " <> t1 <> "=" <> show n1 <> " /= " <> t2 <> "=" <> show n2

fullColumnRank :: Monad m => Text -> LA.Matrix Double -> ASMLH m ()
fullColumnRank mT m = do
  let rank = LA.rank m
      cols = LA.cols m
  when (rank < cols) $ throwASM $ "LSI: rank(" <> mT <> ")=" <> show rank <> " < " <> show cols <> "=cols(" <> mT <> ")!"

fullRowRank :: Monad m => Text -> LA.Matrix Double -> ASMLH m ()
fullRowRank mT m = do
  let rank = LA.rank m
      rows = LA.cols m
  when (rank < rows) $ throwASM $ "LSI: rank(" <> mT <> ")=" <> show rank <> " < " <> show rows <> "=rows(" <> mT <> ")!"


data MDir = Rows | Cols | Square deriving (Show, Eq)

fullRank :: Monad m => Text -> LA.Matrix Double -> ASMLH m ()
fullRank mT m = do
  let (rows, cols) = LA.size m
  let minDir = case compare rows cols of
        EQ -> Square
        LT -> Rows
        GT -> Cols
      fullRankN = min rows cols
      rank = LA.rank m
  when (rank < fullRankN)
    $ throwASM
    $ "LSI: rank(" <> mT <> ")=" <> show rank <> " < " <> show fullRankN <> "=smaller dimension (" <> show minDir <>") of" <> mT <> "!"


{-
-- minimize ||Ex - f|| given inequality constraints
-- transform to LSI (minimize ||z|| w.r.t. different inequality constraints)
-- output is new inequality constraints and function to transform LDP result
-- back to x of given LSI problem
lsiToLDPInequalityConstraints :: Monad m
                              => LogF m
                              -> ActiveSetConfiguration
                              -> LA.Matrix Double
                              -> LA.Vector Double
                              -> InequalityConstraints
                              -> X.ExceptT Text m (InequalityConstraints, LA.Vector Double -> LA.Vector Double)
lsiToLDPInequalityConstraints logF config e f ic = do
  let log = when (asDebug config) . logF
      (m2, n) = LA.size e
      (q, s, k) = LA.svd e
--      k' = LA.tr k
      rank = LA.ranksv (asEpsilon config) (min m2 n) (VS.toList s)
  when (rank < n) $ X.throwE "lsiToLDPInequalityConstraints: given matrix E has rank < cols(E)"
  let rInv = LA.diag $ VS.map (1 /) $ VS.slice 0 n s
      (IC g h) = convertInequalityConstraints ic
      kRinv = k LA.<> rInv
      gLDP = g LA.<> kRinv
      q1 = LA.subMatrix (0, 0) (m2, n) q
      f1 = LA.tr q1 #> f
      hLDP = h - gLDP #> f1
      newInequalityConstraints = MatrixLower gLDP hLDP
      zTox z = kRinv #> (z + f1)
  pure (newInequalityConstraints, zTox)
-}

data LSI_E = Original (LA.Matrix Double)
           | Precomputed (LA.Matrix Double) (LA.Matrix Double) (LA.Matrix Double)

originalE :: LSI_E -> LA.Matrix Double
originalE (Original e) = e
originalE (Precomputed _ _ e) = e

precomputeFromE' :: Monad m => ActiveSetConfiguration -> LA.Matrix Double -> X.ExceptT Text m LSI_E
precomputeFromE' config e = do
  let (m2, n) = LA.size e
      (q, s, k) = LA.svd e
      rank = LA.ranksv (cfgEpsilon config) (min m2 n) (VS.toList s)
  when (rank < n) $ X.throwE "precomputeFromE: given matrix E has rank < cols(E)"
  let rInv = LA.diag $ VS.map (1 /) $ VS.slice 0 n s
      kRinv = k LA.<> rInv
      q1 = LA.subMatrix (0, 0) (m2, n) q
  pure $ Precomputed kRinv q1 e

precomputeFromE :: Monad m => ActiveSetConfiguration -> LA.Matrix Double -> m (Either Text LSI_E)
precomputeFromE config e = X.runExceptT $ precomputeFromE' config e

lsiICAndZtoX :: Monad m
             => ActiveSetConfiguration -> LSI_E -> LA.Vector Double -> InequalityConstraints
             -> X.ExceptT Text m (InequalityConstraints, LA.Vector Double -> LA.Vector Double)
lsiICAndZtoX config (Original e) f ic = precomputeFromE' config e >>= \pc -> lsiICAndZtoX config pc f ic
lsiICAndZtoX _ (Precomputed kRinv q1 _) f ic = do
  let (IC g h) = convertInequalityConstraints ic
      f1 = LA.tr q1 #> f
      gkrI = g LA.<> kRinv
      hLDP = h - gkrI #> f1
      newInequalityConstraints = MatrixLower gkrI hLDP
      zTox z = kRinv #> (z + f1)
  pure (newInequalityConstraints, zTox)


initialLHWorkingSet:: Int -> LH_WorkingSet
initialLHWorkingSet n = LH_WorkingSet $ IM.fromList $ zip [0..(n-1)] (replicate n ASZero)

toState :: AS_State -> [Int] -> LH_WorkingSet -> LH_WorkingSet
toState s is (LH_WorkingSet im) =  LH_WorkingSet $ IM.fromList (zip is (replicate (length is) s)) <> im

workingIS :: LH_WorkingSet -> (IS.IntSet, IS.IntSet)
workingIS (LH_WorkingSet im) = let (fim, zim) = IM.partition (== ASFree) im in (IM.keysSet fim, IM.keysSet zim)

-- keep only columns in the index set
subMatrixC :: IS.IntSet -> LA.Matrix Double -> LA.Matrix Double
subMatrixC is =  subMatrixCL (IS.toList is)
{-# INLINEABLE subMatrixC #-}

subMatrixCL :: [Int] -> LA.Matrix Double -> LA.Matrix Double
subMatrixCL is m =  m LA.?? (LA.All, LA.Pos (LA.idxs is))
{-# INLINEABLE subMatrixCL #-}

-- keep only rows in the index set
subMatrixR :: IS.IntSet -> LA.Matrix Double -> LA.Matrix Double
subMatrixR is =  subMatrixRL (IS.toList is)
{-# INLINEABLE subMatrixR #-}

subMatrixRL :: [Int] -> LA.Matrix Double -> LA.Matrix Double
subMatrixRL is m =  m LA.?? (LA.Pos (LA.idxs is), LA.All)
{-# INLINEABLE subMatrixRL #-}

subMatrixL :: [Int] -> LA.Matrix Double -> LA.Matrix Double
subMatrixL is m =  let idxs = LA.idxs is in m LA.?? (LA.Pos idxs, LA.Pos idxs)
{-# INLINEABLE subMatrixL #-}

subMatrix :: IS.IntSet -> LA.Matrix Double -> LA.Matrix Double
subMatrix is m =  let idxs = LA.idxs (IS.toList is) in m LA.?? (LA.Pos idxs, LA.Pos idxs)
{-# INLINEABLE subMatrix #-}

-- keep only elts in the index set
subVector :: IS.IntSet -> LA.Vector Double -> LA.Vector Double
subVector is = VS.ifilter (\n _ -> IS.member n is)
{-# INLINEABLE subVector #-}

subVectorL :: [Int] -> LA.Vector Double -> LA.Vector Double
subVectorL is = subVector (IS.fromList is)
{-# INLINEABLE subVectorL #-}

data AZAcc = AZAcc { azNextStart :: !Int, azSoFar :: !Int, azVecs :: ![VS.Vector Double]}

-- add zeroes at the indices given
addZeroesV' :: IS.IntSet -> LA.Vector Double -> LA.Vector Double
addZeroesV' is v = g $ IS.foldl' f (AZAcc 0 0 []) is where
  f (AZAcc s sf vecs) i = AZAcc s' sf' vecs'
    where l = i - sf
          s' = s + l
          sf' = sf + l + 1
          vecs' = VS.singleton 0 : VS.unsafeSlice s l v : vecs
  g (AZAcc s _ vecs) = VS.concat . reverse $ (VS.unsafeDrop s v : vecs)
{-# INLINEABLE addZeroesV' #-}
{-
addZeroesV'' :: IS.IntSet -> VS.Vector Double -> VS.Vector Double
addZeroesV'' v xs = VS.convert $ VU.foldMap f $ VU.indexed $ VU.convert xs
  where
    f :: (Int, Double) -> VU.Vector Double
    f (idx, x)
      | IS.member idx v = VU.fromList [0, x]
      | otherwise = VU.singleton x
{-# INLINEABLE addZeroesV'' #-}
-}
-- add zeroes at the indices given
addZeroesV :: IS.IntSet -> LA.Vector Double -> LA.Vector Double
addZeroesV is v = VS.unfoldrExactN (IS.size is + VS.length v) f (0, 0) where
  f (si, di) = if IS.member di is then (0, (si, di + 1)) else (v VS.! si, (si + 1, di + 1))
{-# INLINEABLE addZeroesV #-}

addZeroesVL :: [Int] -> LA.Vector Double -> LA.Vector Double
addZeroesVL is = addZeroesV (IS.fromList is)
{-# INLINEABLE addZeroesVL #-}

modifyAlgoData :: Monad m => (a -> a) -> ASM a m ()
modifyAlgoData f = algoData %= f

nnlsX :: Monad m => (a -> LA.Vector Double) -> ASM a m (LA.Vector Double)
nnlsX xFromAlgoData = xFromAlgoData <$> use algoData

getAlgoData :: Monad m => ASM a m a
getAlgoData = use algoData

getIters :: Monad m => ASM a m Int
getIters = use algoIters

-- returns old iters
nnlsIterPlusOne :: Monad m => ASM a m ()
nnlsIterPlusOne = algoIters %= (+1) >> pure ()

logASM' :: Monad m => Logging -> LogF m ->  Text -> ASM a m ()
logASM' LogAll lf t =  ASM $ lift $ lift $ lf t
logASM' LogOnError _ ~t = RWS.tell $ Seq.singleton t
logASM' LogNone _ ~_ = pure ()

logASM :: Monad m => LogF m -> Text -> ASM a m ()
logASM lf ~t = RWS.asks cfgLogging >>= \l -> logASM' l lf t

logStepLH_NNLS :: Monad m => LogF m -> Text ->  ASMLH m ()
logStepLH_NNLS lf ~t =  RWS.asks cfgLogging >>= \case
  LogNone -> pure ()
  l -> do
    x <- use (algoData . lhX)
    (freeIS, zeroIS) <- indexSets
    n <- getIters
    logASM' l lf $ t <> " (n=" <> show n <> "): "
      <> "; xFree=" <> show (subVector freeIS x)
      <> "; freeI=" <> show (IS.toList freeIS)
      <> "; zeroI=" <> show (IS.toList zeroIS)

indexSets :: Monad m => ASMLH m (IS.IntSet, IS.IntSet)
indexSets = uses (algoData . lhWS) workingIS

lhNNLSStep :: Monad m => LogF m -> LA.Matrix Double -> LA.Vector Double -> NNLS_LHContinue -> ASMLH m (NNLSStep NNLS_LHContinue)
lhNNLSStep logF a b lhc = do
  let log = logASM logF . (" " <>)
      logStep = logStepLH_NNLS logF
      updateX = assign (algoData . lhX)
      getX = use (algoData. lhX)
--      getWS = use (algoData. lhWS)
  epsilon <- RWS.asks cfgEpsilon
  case lhc of
    LH_Setup -> do
      algoData . lhAtb %= const (Just $ LA.tr a #> b)
      algoData . lhAtA %= const (Just $ LA.tr a <> a)
      RWS.asks cfgStart >>= pure . \case
        StartZero -> NNLS_Continue LH_NewFeasible
        StartGS -> NNLS_Continue (LH_GaussSeidel 1)
    LH_GaussSeidel n -> do
      logStep "Gauss-Seidel"
      x <- getX
      ws <- use (algoData . lhWS)
--      ata <- fromJust <$> use (algoData . lhAtA)
--      atb <- fromJust <$> use (algoData . lhAtb)
--      let x' = gaussSeidelIteration ata atb x
      x' <- ASM $ RWS.mapExceptT id $ gaussSeidel a b x
      let ws' = workingSetFromX epsilon x'
          (freeIS', _) = workingIS ws'
      updateX x'
      assign (algoData . lhWS) ws'
      pure $ if ws' == ws || n == 1
             then if IS.size freeIS' == 0
                  then NNLS_Continue LH_NewFeasible -- gotta free something before we try solving
                  else NNLS_Continue (LH_UnconstrainedSolve Nothing)
             else NNLS_Continue (LH_GaussSeidel $ n - 1)

    LH_NewFeasible -> do
      logStep "Feasible"
--      atb <- fromJust <$> use (algoData . lhAtb)
      let w x = LA.tr a LA.#> (b - a LA.#> x)
      use (algoData . lhX) >>= pure . NNLS_Continue . LH_TestW . w
    LH_TestW w  -> do
      logStep "TestW"
      log $ "w=" <> show w
      (freeIS, zeroIS) <- indexSets
      let emptyZ = IS.size zeroIS == 0
          wAtZeros = subVector zeroIS w
          allNegW =  isNothing $ VS.find (> epsilon) wAtZeros
      if emptyZ || allNegW
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
--      log $ "newA=" <> show newA
      solver <- RWS.asks cfgSolver
      lsSolution <- case solver of
        SolveSq -> do
          ata <- fromJust <$> use (algoData . lhAtA)
          atb <- fromJust <$> use (algoData . lhAtb)
          let newAtA = subMatrix freeIS ata
              newAtb = subVector freeIS atb
          case LA.linearSolve newAtA (LA.asColumn newAtb) of
            Nothing -> throwASM $ "Singular A'A in UnconstrainedSolve!" <> show newAtA
            Just s -> pure $ LA.flatten s
        SolveLS -> do
          let newA = subMatrixC freeIS a
          pure $ LA.flatten $ LA.linearSolveLS newA (LA.asColumn b)
        SolveSVD -> do
          let newA = subMatrixC freeIS a
          pure $ LA.flatten $ LA.linearSolveSVD newA (LA.asColumn b)
        SolveQR -> throwASM "SolveQR not supported." --fst $ IQR.incrementalSolveOD (New newAtA) newAtb
      let solWZeros = addZeroesV zeroIS lsSolution
          g (w, t) = if solWZeros VS.! t < negate epsilon then Just (w ,t) else Nothing
      nnlsIterPlusOne
      case mW >>= g of
        Just (w, t) -> do
          log $ "solution=" <> show solWZeros <> " is < 0 at index of max W. Zeroing W there and trying again."
          pure $ NNLS_Continue $ LH_TestW (w VS.// [(t, 0)])
        Nothing ->  case vecIndexSmallest lsSolution of
                      Nothing -> pure $ NNLS_Error "lhNNLSStep: vecIndexSmallest returned Nothing. Empty lsSolution?"
                      Just (_, smallest) -> if smallest > negate epsilon
                        then updateX solWZeros >> pure (NNLS_Continue LH_NewFeasible)
                        else pure $ NNLS_Continue $ LH_NewInfeasible lsSolution
    LH_NewInfeasible zF -> do
      logStep "Infeasible"
      log $ "z=" <> show zF
      (freeIS, zeroIS) <- indexSets
      x <- getX
--      LH_WorkingSet ws <- getWS
      let xF = subVector freeIS x
          alphaF = VS.zipWith (\xx zz -> xx / (xx - zz)) xF zF
          s = zip3 (VS.toList alphaF) (VS.toList zF) (VS.toList xF)
          fstS (q, _, _) = q
          sndS (_, q, _) = q
      log $ "[(alpha, z, x)]=" <> show s
      let s' = sortOn fstS $ filter ((<= epsilon) . sndS) s
      log $ "sorted [(alpha, z<=0, x)]=" <> show s'
      let mAlpha = fstS <$> viaNonEmpty head s'
      case mAlpha of
        Nothing -> pure $ NNLS_Error "lhNNLSStep:Empty list in alpha finding step in LH_NewInFeasible"
        Just alpha -> do
          log $ "alpha=" <> show alpha
          when (alpha > 1 || alpha <= 0) $ throwASM "alpha issue!"
          let x' = x + VS.map (* alpha) (addZeroesV zeroIS zF - x)
              newZeroIs = fmap snd $ filter ((< epsilon) . fst) $ zip (VS.toList x') [0..]
          updateX x'
          algoData . lhWS %= toState ASZero newZeroIs
          pure $ NNLS_Continue $ LH_UnconstrainedSolve Nothing


workingSetFromX :: Double -> LA.Vector Double -> LH_WorkingSet
workingSetFromX epsilon x = LH_WorkingSet im where
  st y = if abs y < epsilon then ASZero else ASFree
  st' (k, y) = (k, st y)
  im = IM.fromList $ fmap st' $ zip [0..] (VS.toList x)

residuals :: LA.Matrix Double -> LA.Vector Double -> LA.Vector Double -> LA.Vector Double
residuals a b x = (a #> x) - b

gradient :: LA.Matrix Double -> LA.Vector Double -> LA.Vector Double -> LA.Vector Double
gradient a b x = LA.tr a #> residuals a b x

gaussSeidel :: Monad m => LA.Matrix Double -> LA.Vector Double -> LA.Vector Double -> X.ExceptT Text m (LA.Vector Double)
gaussSeidel a b x = do
  let sumSqA = VS.fromList $ (\y -> y * y) . LA.norm_2 <$> LA.toColumns a
      q = VS.zipWith (/) (gradient a b x) sumSqA
      g a' b' = if b' > 0 then min 1 (a' / b') else 1
      lambdas = VS.zipWith g x q
--      mLambda = viaNonEmpty head $ sortOn negate $ filter (\y -> y >= 0 && y <= 1) $ VS.toList lambdas
{-  l <- case mLambda of
    Nothing -> X.throwE "No viable lambda in Gauss-Seidel"
    Just l -> pure l
-}
  pure $ x - VS.zipWith (*) lambdas q

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


isFeasible :: Double -> EqualityConstraints -> IC -> LA.Vector Double -> Bool
isFeasible eps' ec ic x =  isEqual eps' ec x && isLowerBound eps' ic x
--  eV = isNothing (VS.find ((> eps') . abs) $ (g #> x) - h)
--  iV = isNothing (VS.find ((< negate eps')) $ (c #> x) - d)

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

jacobiOne :: Int -> LA.Vector Double -> Double -> LA.Vector Double -> Double
jacobiOne k aRow bk x = bk - (ak_dot_x' / aDiag)
  where
    aDiag = aRow VS.! k
    ak_dot_x' = aRow `LA.dot` x - (aDiag * x VS.! k)

jacobiIteration :: LA.Matrix Double -> LA.Vector Double -> LA.Vector Double -> LA.Vector Double
jacobiIteration a b x0 = VS.fromList $ L.zipWith3 f [0..(LA.rows a - 1)] (LA.toRows a) (VS.toList b)
  where f k aRow bk = jacobiOne k aRow bk x0


gaussSeidelIteration :: LA.Matrix Double -> LA.Vector Double -> LA.Vector Double -> LA.Vector Double
gaussSeidelIteration a b x0 = FL.fold gsFld $ zip3 [0..(LA.rows a - 1)] (LA.toRows a) (VS.toList b)
  where
    initial = x0
    step gsX (k, aRow, bk) = let xk' = jacobiOne k aRow bk gsX in gsX VS.// [(k, xk')]
    gsFld = FL.Fold step initial id


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
