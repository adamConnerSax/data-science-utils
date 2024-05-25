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
