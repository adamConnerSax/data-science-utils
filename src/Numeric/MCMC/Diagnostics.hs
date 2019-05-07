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
  , ExpectationSummary(..)
  )
where

import qualified Control.Foldl                 as FL
import qualified Statistics.Types              as S
import qualified Data.List                     as L

data ExpectationSummary a = ExpectationSummary { mean :: a, confidence :: (a, a), rHat :: a } deriving (Show)
-- all sub-chains should have same length here
-- do we need to thrwo away first half of all the chains??
summarize
  :: (RealFrac b, Ord b)
  => S.CL b -- ^ confidence interval to test
  -> (a -> b) -- ^ function to map a sample to a function under consideration
  -> [[a]] -- ^ list of chains
  -> Maybe (ExpectationSummary b) -- ^ @Maybe@ here in case chains aren't long enough
summarize confidence expectationF chains = do
  let applied = fmap (fmap expectationF) chains
      chainInterval :: Ord b => [b] -> Maybe (b, b)
      chainInterval c =
        let n  = L.length c
            l  = round (realToFrac n * S.confidenceLevel confidence)
            c' = L.take l $ L.drop ((n - l) `div` 2) $ L.sort c
        in  if length c' >= 2 then Just (head c', last c') else Nothing
      intervalLength (a, b) = b - a
  intervalLengths <- traverse (fmap intervalLength . chainInterval) applied
  let allApplied = concat applied
  (lo, hi) <- chainInterval allApplied
  let rHat = (hi - lo) / (FL.fold FL.mean intervalLengths)
      mean = FL.fold FL.mean allApplied
  return $ ExpectationSummary mean (lo, hi) rHat




