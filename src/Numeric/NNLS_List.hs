module Numeric.NNLS_List where

import Data.List (sort, sortBy)
import qualified Data.List as List
import Data.Ord (comparing)
import Data.Maybe (fromMaybe)

-- Function to calculate the solution with linear equality constraints
nnlsEqConstr :: (Num a, Ord a) => [[a]] -> [a] -> [[a]] -> [a] -> [a]
nnlsEqConstr a b c d = nnlsEqConstrActive a b c d [] (length $ List.head a)

-- Active set method for NNLS with equality constraints
nnlsEqConstrActive :: (Num a, Ord a) => [[a]] -> [a] -> [[a]] -> [a] -> [Int] -> Int -> [a]
nnlsEqConstrActive a b c d activeSet n
  | n == 0 = replicate (length b) 0
  | otherwise = let
      (z, activeSet', n') = nnlsEqConstrStep a b c d activeSet n
    in
      if n' == 0
        then z
        else nnlsEqConstrActive a b c d activeSet' n'

-- NNLS step with equality constraints
nnlsEqConstrStep :: (Num a, Ord a) => [[a]] -> [a] -> [[a]] -> [a] -> [Int] -> Int -> ([a], [Int], Int)
nnlsEqConstrStep a b c d activeSet n = let
    (x, newActiveSet) = solveSubproblemWithEqConstraints a b c d activeSet
    newZ = zipWith max x (replicate n 0)
    violatedSet = findViolatedSet newZ activeSet
  in
    if null violatedSet
      then (newZ, [], 0)
      else (newZ, newActiveSet ++ violatedSet, length violatedSet)

-- Solve the subproblem with equality constraints
solveSubproblemWithEqConstraints :: (Num a, Ord a) => [[a]] -> [a] -> [[a]] -> [a] -> [Int] -> ([a], [Int])
solveSubproblemWithEqConstraints a b c d activeSet = let
    (x', _) = nnlsQPWithEqConstr activeA activeC b' d'
  in
    (x', sort $ map snd $ filter (not . (> 0) . fst) $ zip x' [0 ..])
  where
    activeA = [a List.!! i | i <- activeSet]
    activeC = [c List.!! i | i <- activeSet]
    b' = b `mvMult` transpose activeA
    d' = d `mvMult` transpose activeC
    col :: [[a]] -> Int -> [a]
    col xs i = map (!! i) xs

-- Find violated set
findViolatedSet :: (Num a, Ord a) => [a] -> [Int] -> [Int]
findViolatedSet z activeSet =
  map snd $
    sortBy (comparing fst) $
      filter ((< 0) . fst) $
        zip z [0 .. (length z - 1)]

-- Helper functions for matrix operations
mvMult :: Num a => [[a]] -> [a] -> [a]
mvMult a b = map (sum . zipWith (*) b) (transpose a)

nnlsQPWithEqConstr :: (Num a, Ord a) => [[a]] -> [[a]] -> [a] -> [a] -> ([a], [[a]])
nnlsQPWithEqConstr a c b d = let
    m = length a
    n = length b
    p = length c
    augA = a ++ transpose c
    augB = b ++ d
    solution = fromMaybe (replicate (m + p) 0) $ backSub augA augB
    x = take m solution
    y = drop m solution
  in
    (x, [y])

backSub :: (Num a, Ord a) => [[a]] -> [a] -> Maybe [a]
backSub a b =
  if rank a == length b
    then Just $ backSubst (gaussianElimination a b)
    else Nothing

rank :: (Num a, Eq a) => [[a]] -> Int
rank = length . filter (not . null)

backSubst :: (Num a, Ord a) => [[a]] -> [a]
backSubst augmented = reverse $ map last $ foldl backSubstStep [] augmented

backSubstStep :: (Num a, Ord a) => [[a]] -> [a] -> [[a]]
backSubstStep xs [] = xs
backSubstStep xs (x : xs') =
  backSubstStep ((x - sum (zipWith (*) xs' row)) : row : xs) xs'
  where
    row = init $ last xs

gaussianElimination :: (Num a, Ord a) => [[a]] -> [a] -> [[a]]
gaussianElimination a b = foldl swapRows (a ++ [b]) (reverse pivotIndices)
  where
    pivotIndices = map snd $ filter ((/= 0) . fst) $ zip (diag a) [0 ..]
    diag = zipWith (!!) a
    swapRows m i = takeWhile (\x -> x /= i) m ++ [m !! i] ++ dropWhile (\x -> x == i) (drop 1 m)
