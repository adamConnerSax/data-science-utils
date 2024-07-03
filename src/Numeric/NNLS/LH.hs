{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE Strict #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}
module Numeric.NNLS.LH
  (
    module Numeric.NNLS.LH
  , module Numeric.NNLS.Types
  ) where


import Numeric.Optimization.NLOPT.NNLS as NNLS

import Numeric.NNLS.Types
import qualified Numeric.LinearAlgebra as LA
import Numeric.LinearAlgebra ((#>))
import qualified Data.Vector.Storable as VS

data NNLSResult = Success (LA.Vector Double) Double | WrongDimensions | TooManyIterations | IncompatibleInequalities | ENotFullRank
  deriving stock (Show, Eq)

nnlsResult :: (LA.Vector Double, Double, Int) -> NNLSResult
nnlsResult (x, norm, mode) = case mode of
  1 -> Success x norm
  2 -> WrongDimensions
  3 -> TooManyIterations
  4 -> IncompatibleInequalities
  5 -> ENotFullRank

nnlsResE :: NNLSResult -> Either Text (LA.Vector Double)
nnlsResE = \case
  Success x _ -> pure x
  y -> Left $ show y

-- hmatrix flattens to row major but we want flat column-major
-- returns the vector pass well as the leading dimension and the rows and columns
data NNLSMatrix = NNLSMatrix { vec :: !(LA.Vector Double), rows :: Int, columns :: Int, leadingDim :: Int}

nnlsMatrix :: LA.Matrix Double -> NNLSMatrix
nnlsMatrix m = let (r, c) = LA.size m
               in NNLSMatrix (LA.flatten $ LA.tr m) r c r

optimalNNLS :: LA.Matrix Double
            -> LA.Vector Double
            -> IO NNLSResult
optimalNNLS a b  = do
  let nnlsM = nnlsMatrix a
  r <- NNLS.nnls (rows nnlsM) (columns nnlsM) (vec nnlsM) b
  pure $ nnlsResult r


optimalLDP :: InequalityConstraints
           -> IO NNLSResult
optimalLDP ic' = do
  let (IC g h) = convertInequalityConstraints ic'
      nnlsG = nnlsMatrix g
  r <- NNLS.ldp (rows nnlsG) (columns nnlsG) (vec nnlsG) h
  pure $ nnlsResult r


optimalLSI :: LA.Matrix Double
           -> LA.Vector Double
           -> InequalityConstraints
           -> IO NNLSResult
optimalLSI e f ic = do
  let (IC g h) = convertInequalityConstraints ic
      nnlsE = nnlsMatrix e
      nnlsG = nnlsMatrix g
  r <- lsi (columns nnlsE) (rows nnlsE) (rows nnlsG) (vec nnlsE) f (vec nnlsG) h
  pure $ nnlsResult r

data LSI_E = Original (LA.Matrix Double)
           | Precomputed (LA.Matrix Double) (LA.Matrix Double) NNLSMatrix

optimalLSI' :: LSI_E
            -> LA.Vector Double
            -> InequalityConstraints
            -> IO NNLSResult
optimalLSI' lsiE f ic = case lsiE of
  Original e -> optimalLSI e f ic
  pc@(Precomputed _ _ _) -> do
    let (icz, zTox) = lsiICAndZtoX pc f ic
    r <- optimalLDP icz
    case r of
      Success z zNorm -> pure $ Success (zTox z) zNorm
      x -> pure x

originalE :: LSI_E -> NNLSMatrix
originalE (Original e) = nnlsMatrix e
originalE (Precomputed _ _ e) = e

precomputeFromE :: LA.Matrix Double -> LSI_E
precomputeFromE e = do
  let (m2, n) = LA.size e
      (q, s, k) = LA.svd e
      rInv = LA.diag $ VS.map (1 /) $ VS.slice 0 n s
      kRinv = k LA.<> rInv
      q1 = LA.subMatrix (0, 0) (m2, n) q
  Precomputed kRinv q1 (nnlsMatrix e)

lsiICAndZtoX :: LSI_E -> LA.Vector Double -> InequalityConstraints
             -> (InequalityConstraints, LA.Vector Double -> LA.Vector Double)
lsiICAndZtoX (Original e) f ic = lsiICAndZtoX (precomputeFromE e) f ic
lsiICAndZtoX (Precomputed kRinv q1 _) f ic =
  let (IC g h) = convertInequalityConstraints ic
      f1 = LA.tr q1 #> f
      gkrI = g LA.<> kRinv
      hLDP = h - gkrI #> f1
      newInequalityConstraints = MatrixLower gkrI hLDP
      zTox z = kRinv #> (z + f1)
  in (newInequalityConstraints, zTox)




{-
nnlsCheckDims :: LA.Matrix Double
              -> LA.Vector Double
              -> Either Text ()
nnlsCheckDims a b = do
  let (aRows, aCols) = LA.size a
      bLength = LA.size b
  checkPair "NNLS" (aRows, "rows(A)") (bLength, "length(b)")




ldpCheckDims :: IC -> Either Text ()
ldpCheckDims (IC g h) = do
  let gRows = LA.rows g
      hLength = LA.size h
  checkPair "LDP" (gRows, "rows(G)") (hLength, "length(h)")
-}
{-

lsiAlgo :: LSI_E
        -> LA.Vector Double
        -> InequalityConstraints
        -> LogF m
        -> Either Text (LA.Vector Double)
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

lsiCheckDimsAndRank :: Monad m => LSI_E -> LA.Vector Double -> InequalityConstraints -> Either Text ()
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
-}

checkPair :: Text -> (Int, Text) -> (Int, Text) ->  Either Text ()
checkPair t (n1, t1) (n2, t2) =
  when (n1 /= n2)
  $ Left
  $ "Inconsistent dimensions in " <> t <> ": " <> t1 <> "=" <> show n1 <> " /= " <> t2 <> "=" <> show n2

fullColumnRank :: Text -> LA.Matrix Double -> Either Text ()
fullColumnRank mT m = do
  let rank = LA.rank m
      cols = LA.cols m
  when (rank < cols) $ Left $ "LSI: rank(" <> mT <> ")=" <> show rank <> " < " <> show cols <> "=cols(" <> mT <> ")!"

fullRowRank :: Text -> LA.Matrix Double -> Either Text ()
fullRowRank mT m = do
  let rank = LA.rank m
      rows = LA.cols m
  when (rank < rows) $ Left $ "LSI: rank(" <> mT <> ")=" <> show rank <> " < " <> show rows <> "=rows(" <> mT <> ")!"


data MDir = Rows | Cols | Square deriving (Show, Eq)

fullRank :: Text -> LA.Matrix Double -> Either Text ()
fullRank mT m = do
  let (rows, cols) = LA.size m
  let minDir = case compare rows cols of
        EQ -> Square
        LT -> Rows
        GT -> Cols
      fullRankN = min rows cols
      rank = LA.rank m
  when (rank < fullRankN)
    $ Left
    $ "LSI: rank(" <> mT <> ")=" <> show rank <> " < " <> show fullRankN <> "=smaller dimension (" <> show minDir <>") of" <> mT <> "!"

{-

-}
