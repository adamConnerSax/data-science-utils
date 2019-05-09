{-|
Module      : Numeric.MCMC.Diagnostics
Description : Diagnostics for Markov Chain Monte Carlo Sampling
Copyright   : (c) Adam Conner-Sax 2019
License     : BSD
Maintainer  : adam_conner_sax@yahoo.com
Stability   : experimental

Numeric.MCMC.Diagnostics contains functions to analyze the convergence of MCMC sampling and to estimate the distribution of expectations computed
using a set of MCMC chains.
-}
module Numeric.MCMC.Diagnostics
  ( summarize
  , mpsrf
  , ExpectationSummary(..)
  , mannWhitneyUTest
  )
where

import qualified Control.Foldl                 as FL
import qualified Statistics.Types              as S
import qualified Data.List                     as L
import qualified Data.Vector.Unboxed           as VU
import qualified Statistics.Test.MannWhitneyU  as S
import qualified Numeric.LinearAlgebra         as LA

data ExpectationSummary a = ExpectationSummary { mean :: a, confidence :: (a, a), rHat :: a } deriving (Show)
-- all sub-chains should have same length here
-- do we need to throw away first half of all the chains??
{- |
Compute the exepctation, confidence interval and
PSRF statistic (from <http://www2.stat.duke.edu/~scs/Courses/Stat376/Papers/ConvergeDiagnostics/BrooksGelman.pdf>)
for a given confidence %, expectation function, and set of MCMC chains. In the version in the paper, each chain is
halved before the statistic is computed but it's not clear why.  Here we assume that is "burn-in" and the user may
choose to do that or not before calling this function.

All sub-chains should have the same length here, though that is not checked.
-}
summarize
  :: (RealFrac b, Ord b)
  => S.CL b -- ^ confidence interval to test
  -> (a -> b) -- ^ function to map a sample to a function under consideration
  -> [[a]] -- ^ list of chains
  -> Maybe (ExpectationSummary b) -- ^ @Maybe@ here in case chains aren't long enough
summarize confidence expectationF chains = do
  let applied    = fmap (fmap expectationF) chains
      allApplied = concat applied
      chainInterval :: Ord b => [b] -> Maybe (b, b)
      chainInterval c =
        let n  = L.length c
            n' = round (realToFrac n * S.confidenceLevel confidence)
            d  = (n - n') `div` 2
            c' = L.take n' $ L.drop d $ L.sort c
        in  if length c' >= 2 then Just (head c', last c') else Nothing
--      intervalLength (a, b) = b - a
  intervals <- traverse chainInterval applied
  (lo, hi)  <- chainInterval allApplied
  let (mLo, mHi) = FL.fold
        ((,) <$> FL.premap fst FL.mean <*> FL.premap snd FL.mean)
        intervals
  let rHat = (hi - lo) / (mHi - mLo)
      mean = FL.fold FL.mean allApplied
  return $ ExpectationSummary mean (lo, hi) rHat

-- if the chain list is empty or chains have different sizes, etc. we return Nothing
mpsrf :: [a -> Double] -> [[a]] -> Maybe Double
mpsrf expectations chains = do
  let --mkVec :: a -> LA.Vector Double
      mkVec x = LA.fromList $ fmap ($ x) expectations
      m = length chains
  n <- if (m > 0) then Just (length $ head chains) else Nothing
  _ <- traverse (\c -> if length c == n then Just () else Nothing) chains -- check that all chains are length n
  let
    sumVecsF = FL.Fold (+) (LA.vector $ L.replicate (length expectations) 0) id
    sumMatsF = FL.Fold (+) (LA.scale 0 $ LA.ident (length expectations)) id
    meanVecsF =
      (\m l -> LA.scale (1 / realToFrac l) m) <$> sumVecsF <*> FL.length
    avgVec     = FL.fold meanVecsF
    phiChains  = fmap (fmap mkVec) chains
    avgPhiEach = fmap avgVec phiChains
    diffProd :: LA.Vector Double -> LA.Vector Double -> LA.Matrix Double
    diffProd x y = (x - y) `LA.outer` (x - y)
    w1 = fmap (\(ap, ps) -> fmap (\p -> diffProd p ap) ps)
      $ zip avgPhiEach phiChains -- [[(p - ap)(p - ap)']]
    w =
      LA.scale (1 / (realToFrac m * realToFrac (n - 1)))
        $ FL.fold sumMatsF
        $ fmap (FL.fold sumMatsF) w1
    avgPhiAll = avgVec avgPhiEach
    b1        = fmap (\p -> diffProd p avgPhiAll) avgPhiEach
    b = LA.scale (realToFrac n / realToFrac (m - 1)) $ FL.fold sumMatsF b1
    mat       = LA.scale (1 / realToFrac n) $ LA.inv w LA.<> b
    eigen1    = LA.eigenvaluesSH (LA.trustSym mat) `LA.atIndex` 0
    rhat =
      (realToFrac (n - 1) / realToFrac n)
        + (realToFrac (m + 1) / realToFrac m)
        * eigen1
  return rhat

mannWhitneyUTest
  :: (Ord b, VU.Unbox b)
  => S.PValue Double
  -> (a -> b)
  -> [a]
  -> [a]
  -> Maybe S.TestResult
mannWhitneyUTest p f c1 c2 =
  let vOfF = VU.fromList . fmap f
  in  S.mannWhitneyUtest S.SamplesDiffer p (vOfF c1) (vOfF c2)
