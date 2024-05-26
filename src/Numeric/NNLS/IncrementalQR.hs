{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
module Numeric.NNLS.IncrementalQR where

import qualified Numeric.LinearAlgebra as LA
import Numeric.LinearAlgebra ((|||), (===), (#>), (<#))
import qualified Data.Vector.Storable as VS

-- build a solver for Ax = b  which can be clever when re-solving for A' which is one column or row different from A

data PrevQR = PrevQR { thinQ :: !(LA.Matrix Double), thinR :: !(LA.Matrix Double), invThinR :: !(LA.Matrix Double) }

data IncrementalA = New (LA.Matrix Double)
                  | Same PrevQR
                  | AddRow PrevQR (LA.Vector Double)
                  | AddColumn PrevQR (LA.Vector Double)

-- over determined. A is full rank and has more rows than columns
incrementalSolveOD :: IncrementalA -> LA.Vector Double -> (LA.Vector Double, PrevQR)
incrementalSolveOD (New a) b =
  let (tQ, tR) = LA.thinQR a
      iTR = LA.triSolve LA.Upper tR (LA.ident $ LA.rows tR)
  in incrementalSolveOD (Same $ PrevQR tQ tR iTR) b
incrementalSolveOD (Same pqr@(PrevQR tQ _ iR)) b = (iR #> LA.tr tQ #> b, pqr)


{-
-- Rather than storing \theta, we store cos(\theta) and sin(\theta)
-- since we can compute them directly
data GivensRotation = GivensRotation Int Int Double Double

givensCS :: Double -> Double -> (Double, Double)
givensCS a b
  | b == 0 = (1, 0)
  | abs b > abs a = let tau = negate a / b; s = 1 / sqrt ( 1 + tau^2) in (s * tau, s)
  | otherwise = let tau = negate a / b; c = 1 / sqrt ( 1 + tau^2) in (c, c * tau)
{-# INLINEABLE givensCS #-}

zeroColAtkPlusOneG :: Int -> Int -> LA.Matrix Double -> GivensRotation
zeroColAtkPlusOneG col k m =
  let col = m LA.! col
      (c, s) = givensCS (col LA.! k) (col LA.! (k + 1))
  in GivensRotation k (k + 1) c s

-}
