{-# LANGUAGE ScopedTypeVariables #-}
module Numeric.ActiveSet where

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
  b = VS.concat [l, negate u]
convertInequalityConstraints (MatrixUpper a b) = IC (negate a) (negate b)
convertInequalityConstraints (MatrixLower a b) = IC a b

data WorkingSets = WorkingSets { active :: !(Set Int), violated :: !(Set Int) }

-- given a set of inequality constraints, find active and violated sets for a given vector
workingSets :: Double -> IC -> LA.Vector Double -> WorkingSets
workingSets eps (IC a b) u =  f $ VS.foldl' g (0, mempty, mempty) $ (a #> u) - b
  where
    g (n, a', v') x
      | abs x < eps = (n+ 1, S.insert n a', v')
      | x < -eps = (n + 1, a', S.insert n v')
      | otherwise = (n + 1, a', v')
    f (_, a', v') = WorkingSets a' v'

-- solve ||Ax - b||_2 s.t. Cx = d and produce Least-Squares x as well as Lagrange multipliers for constraints.
-- via G(x,l) = ||Ax - b||_2 + l'(Cx - d)
-- dG/dx = 0 and dG/dl = 0
-- solving |2A'A C'| |x| = |2b'A|
--         |C    0 | |l|   |d|
equalityConstrained :: LA.Matrix Double -> LA.Vector Double -> LA.Matrix Double -> LA.Vector Double -> Maybe (LA.Vector Double, LA.Vector Double)
equalityConstrained a b c d = VS.splitAt n . LA.flatten <$> sM  where
  (l, n) = LA.size a
  cRows = LA.rows c
  q = (2 * (LA.tr a) <> a ||| LA.tr c) === (c ||| LA.konst 0 (cRows, cRows))
  y = VS.concat [2 * b <# a, d]
  sM = LA.linearSolve q (LA.asColumn y)


-- for a given set of inequality constraints, a feasible vector x and a perturbation p, find the maximum r
-- s.t., x + r p is feasible as well as the index which limited it
-- we do this by finding the max r for each element and then choosing the smallest of those
maxAlpha :: IC -> LA.Vector Double -> LA.Vector Double -> Maybe (Double, Int)
maxAlpha (IC a b) x p = result where
  rs = VS.zipWith3 (\ax ap b' -> ax - b' / ap) (a #> x) (a #> p) b
  result = fmap (\(a', b') -> (b', a')) $ viaNonEmpty head $ L.sortOn snd $ filter ((> 0) . snd) $ zip [(0 :: Int)..] $ VS.toList rs
