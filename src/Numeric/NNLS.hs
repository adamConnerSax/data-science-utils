{-# LANGUAGE ScopedTypeVariables #-}
module Numeric.NNLS where

import Prelude hiding (toList, fromList)
import Numeric.LinearAlgebra
import qualified Data.Vector.Storable as V
import Data.Maybe (fromJust)
import qualified Control.Foldl as FL
import qualified Data.List as L


-- after Chen & Ye: https://arxiv.org/pdf/1101.6081
projectToSimplex :: V.Vector Double -> V.Vector Double
projectToSimplex y = V.fromList $ fmap (\x -> max 0 (x - tHat)) yL
  where
    yL = V.toList y
    n = V.length y
    sY = sort yL
    t i = (FL.fold FL.sum (L.drop i sY) - 1) / realToFrac (n - i)
    tHat = go (n - 1)
    go 0 = t 0
    go k = let tk = t k in if tk > sY L.!! k then tk else go (k - 1)

-- Function to calculate the solution with linear equality constraints
nnlsEqConstr :: Matrix Double -> Vector Double -> Matrix Double -> Vector Double -> Vector Double
nnlsEqConstr a b c d = let n = cols a in nnlsEqConstrActive a b c d [0..(n - 1)] n

-- Active set method for NNLS with equality constraints
nnlsEqConstrActive :: Matrix Double -> Vector Double -> Matrix Double -> Vector Double -> [Int] -> Int -> Vector Double
nnlsEqConstrActive a b c d activeSet n
  | n == 0 = konst 0 (size b)
  | otherwise = let
      (z, activeSet', n') = nnlsEqConstrStep a b c d activeSet n
    in
      if n' == 0
        then z
        else nnlsEqConstrActive a b c d activeSet' n'

-- NNLS step with equality constraints
nnlsEqConstrStep :: Matrix Double -> Vector Double -> Matrix Double -> Vector Double -> [Int] -> Int -> (Vector Double, [Int], Int)
nnlsEqConstrStep a b c d activeSet n = let
    (x, newActiveSet) = solveSubproblemWithEqConstraints a b c d activeSet
    newZ = V.zipWith max x (V.replicate n 0)
    violatedSet = findViolatedSet newZ
  in
    if null violatedSet
      then (newZ, [], 0)
      else (newZ, newActiveSet ++ violatedSet, length violatedSet)

-- Solve the subproblem with equality constraints
solveSubproblemWithEqConstraints :: Matrix Double -> Vector Double -> Matrix Double -> Vector Double -> [Int] -> (V.Vector Double, [Int])
solveSubproblemWithEqConstraints a b c d activeSet = let
    activeA = takeColumnsByIndex a activeSet
    activeC = takeColumnsByIndex c activeSet
    activeBVec = subVectorByIndex b activeSet
    activeDVec = subVectorByIndex d activeSet
--    (x', _) = nnlsQPWithEqConstr activeA activeC activeBVec activeDVec
    x' = nnlsQPWithEqConstr activeA activeC activeBVec activeDVec
    newActiveSet = sort $ map fst $ filter ((>= 0) . snd) $ zip [0..] (toList x')
  in
    (x', newActiveSet)

-- Find violated set
findViolatedSet :: V.Vector Double -> [Int]
findViolatedSet z =
  map fst $
    sortBy (comparing snd) $
      filter ((< 0) . snd) $
        zip [0 .. (V.length z - 1)] (toList z)

-- Helper function to solve the quadratic programming problem with equality constraints
nnlsQPWithEqConstr :: Matrix Double -> Matrix Double -> Vector Double -> Vector Double -> V.Vector Double --(V.Vector Double, [Vector Double])
nnlsQPWithEqConstr a c b d = let
    m = rows a
--    n = rows c
    augA = augmentHorizontally a (tr c)
    augB = augmentVertically b d
    solution = linearSolveSVD augA (asColumn augB)
    x = subVector 0 m $ flatten solution
--    y = toRows $ takeRows n solution
  in x
--    (x, [y])

-- Helper functions for augmenting matrices and vectors
augmentHorizontally :: Matrix Double -> Matrix Double -> Matrix Double
augmentHorizontally a b = fromBlocks [[a, b]]

augmentVertically :: Vector Double -> Vector Double -> Vector Double
augmentVertically a b = vjoin [a, b]

-- Helper function for column selection
--takeColumns :: Matrix Double -> [Int] -> Matrix Double
--takeColumns m indices = cmap (`elem` indices) m

takeColumnsByIndex :: Matrix Double -> [Int] -> Matrix Double
takeColumnsByIndex m = fromColumns . byIndex (toColumns m)

subVectorByIndex :: Vector Double -> [Int] -> Vector Double
subVectorByIndex v = fromList . byIndex (toList v)

byIndex :: [a] -> [Int] -> [a]
byIndex l indices = fmap snd . filter ((\n -> n `elem` indices) . fst) $ zip [0..] l
