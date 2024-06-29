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
module Numeric.NNLS.LH
  (
    module Numeric.NNLS.LH
  ) where


import Numeric.Optimization.NLOPT.NNLS as NNLS

import Numeric.NNLS.Types
import qualified Numeric.LinearAlgebra as LA
import Numeric.LinearAlgebra ((|||), (===), (#>), (<#))
import qualified Data.Vector.Storable as VS
import qualified Control.Foldl as FL
import qualified Data.List as L

import qualified Control.Monad.RWS.Strict as RWS
import qualified Control.Monad.Except as RWS
import qualified Control.Error as X

optimalNNLS :: LA.Matrix Double
            -> LA.Vector Double
            -> IO (Either Text (LA.Vector Double))
optimalNNLS a b  = do
  let checkE = nnlsCheckDims a b
  either (pure . Left) (pure . Right) checkE
  let (m, n) = LA.size a
      aVec = LA.flatten $ LA.tr a
  (res, mode) <- NNLS.nnls m n aVec b
  pure $ case mode of
    1 -> Right res
    2 -> Left "Dimension problem in NNLS."
    3 -> Left $ "Too many iterations. > 3*N = " <> show (3 * LA.rows a)

nnlsCheckDims :: LA.Matrix Double
              -> LA.Vector Double
              -> Either Text ()
nnlsCheckDims a b = do
  let (aRows, aCols) = LA.size a
      bLength = LA.size b
  checkPair "NNLS" (aRows, "rows(A)") (bLength, "length(b)")
{-
optimalLDP :: InequalityConstraints
           -> Either Text (LA.Vector Double)
optimalLDP ic' = do
  let (IC g _) = convertInequalityConstraints ic'
      nnlsSize = LA.rows g
  runASM logF config (initialNNLSWorkingDataLH nnlsSize) $ ldpAlgo ic'

ldpCheckDims :: Monad m => IC -> Either Text ()
ldpCheckDims (IC g h) = do
  let gRows = LA.rows g
      hLength = LA.size h
  checkPair "LDP" (gRows, "rows(G)") (hLength, "length(h)")


optimalLSI :: LSI_E
           -> LA.Vector Double
           -> InequalityConstraints
           -> Either Text (LA.Vector Double)
optimalLSI lsiE f ic = do
  let nnlsSize = LA.rows (originalE lsiE)
  runASM logF config (initialNNLSWorkingDataLH nnlsSize) $ lsiAlgo lsiE f ic

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
data LSI_E = Original (LA.Matrix Double)
           | Precomputed (LA.Matrix Double) (LA.Matrix Double) (LA.Matrix Double)


originalE :: LSI_E -> LA.Matrix Double
originalE (Original e) = e
originalE (Precomputed _ _ e) = e

precomputeFromE :: Monad m => ActiveSetConfiguration -> LA.Matrix Double -> Either Text LSI_E
precomputeFromE config e = do
  let (m2, n) = LA.size e
      (q, s, k) = LA.svd e
      rank = LA.ranksv (cfgEpsilon config) (min m2 n) (VS.toList s)
  when (rank < n) $ Left "precomputeFromE: given matrix E has rank < cols(E)"
  let rInv = LA.diag $ VS.map (1 /) $ VS.slice 0 n s
      kRinv = k LA.<> rInv
      q1 = LA.subMatrix (0, 0) (m2, n) q
  pure $ Precomputed kRinv q1 e

lsiICAndZtoX :: Monad m
             => LSI_E -> LA.Vector Double -> InequalityConstraints
             -> Either Text (InequalityConstraints, LA.Vector Double -> LA.Vector Double)
lsiICAndZtoX config (Original e) f ic = precomputeFromE' config e >>= \pc -> lsiICAndZtoX config pc f ic
lsiICAndZtoX _ (Precomputed kRinv q1 _) f ic = do
  let (IC g h) = convertInequalityConstraints ic
      f1 = LA.tr q1 #> f
      gkrI = g LA.<> kRinv
      hLDP = h - gkrI #> f1
      newInequalityConstraints = MatrixLower gkrI hLDP
      zTox z = kRinv #> (z + f1)
  pure (newInequalityConstraints, zTox)
-}
