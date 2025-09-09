{-# LANGUAGE ScopedTypeVariables #-}
module Numeric.NNLS where

import Prelude hiding ((<>))
import qualified Numeric.LinearAlgebra as LA
import Numeric.LinearAlgebra ((|||), (===), (#>), (<#), (<>))
import qualified Data.Set as S
import qualified Data.Vector.Storable as VS
import qualified Control.Foldl as FL
import qualified Data.List as L


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


data InequalityConstraints where
  SimpleBounds :: LA.Vector Double -> LA.Vector Double -> InequalityConstraints
  -- ^ l <= x <= u
  MatrixUpper :: LA.Matrix Double -> LA.Vector Double -> InequalityConstraints
  -- ^ Ax <= b
  MatrixLower :: LA.Matrix Double -> LA.Vector Double -> InequalityConstraints
  -- ^ Ax >= b

data IC = IC (LA.Matrix Double) (LA.Vector Double)

convertInequalityConstraints :: InequalityConstraints -> IC
convertInequalityConstraints (SimpleBounds l u ) = IC a b where
  a = LA.ident (LA.size l) === negate (LA.ident $ LA.size u)
  b = VS.concat [lower, negate upper]
convertInequalityConstraints (MatrixUpper a b) = IC (negate a) (negate b)
convertInequalityConstraints (MatrixLower a b) = IC a b

data WorkingSets = WorkingSets { active :: !(Set Int), violated :: !(Set Int) }

findSets :: Double -> IC -> LA.Vector Double -> WorkingSets
findSets eps (IC a b) u =  f $ VS.foldl' g (0, mempty, mempty) $ a #> u
  where
    g (n, a, v) x
      | abs x < eps = (n+ 1, S.insert n a, v)
      | x < -eps = (n + 1, a, S.insert n v)
      | otherwise = (n + 1, a, v)
    f (_, a, v) = WorkingSets a v

-- solve ||Ax - b|| s.t. Cx = d and produce LS x as well as Lagrange multipliers for constraints
solveEqualityConstrained :: LA.Matrix Double -> LA.Vector Double -> LA.Matrix Double -> LA.Vector Double -> (LA.Vector Double, LA.Vector Double)
solveEqualityConstrained a b c d = (u, lambda) where
  (l, n) = LA.size a
  cRows = LA.rows c
  q = (2 * (LA.tr a) <> a ||| LA.tr c) ==== (c ||| LA.konst 0 (cRows, cRows))
  v = VS.concat [2 * b ># a, d]
  s = q </> v
  (u, lambda) = VS.splitAt n s
