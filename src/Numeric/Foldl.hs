{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Numeric.Foldl where

import qualified Control.Foldl as FL
import qualified Data.List as L

sumWithWeightsF :: (Real b, RealFrac c)
                => (a -> b) -> (a -> c) -> FL.Fold a c
sumWithWeightsF wgt sumOf = FL.premap (\a -> realToFrac (wgt a) * realToFrac (sumOf a)) FL.sum
{-# INLINE sumWithWeightsF #-}

wgtdSumF :: (RealFrac b, RealFrac c)
              => (a -> b) -> (a -> c) -> FL.Fold a c
wgtdSumF wgt sumOf = (/) <$> sumWithWeightsF wgt sumOf <*> fmap realToFrac (FL.premap wgt FL.sum)
{-# INLINE wgtdSumF #-}

data StrictPair a = StrictPair !Double !a
getFst (StrictPair x _) = x
getSnd (StrictPair _ a) = a

weightedMedian :: forall a.(Ord a) => a -> [StrictPair a] -> a
weightedMedian dfltA !l =
  let ordered :: [StrictPair a] = L.sortOn getSnd l
      middleWeight :: Double = FL.fold (FL.premap getFst FL.sum) l / 2
      update :: StrictPair a -> StrictPair a -> (StrictPair a, ())
      update (StrictPair wgtSoFar medianSoFar) (StrictPair w x) = (StrictPair (wgtSoFar + w) newMedian, ()) where
        newMedian = if wgtSoFar <= middleWeight then x else medianSoFar
      -- mapAccumL :: Traversable t => (a -> b -> (a, c)) -> a -> t b -> (a, t c)
      (StrictPair _ res, _) = L.mapAccumL update (StrictPair 0 dfltA) ordered
  in res
{-# INLINE weightedMedian #-}

weightedMedianF :: (Num b, Ord b) => (a -> Double) -> (a -> b) -> FL.Fold a b
weightedMedianF wgt median = weightedMedian 0 <$> FL.premap (\x -> StrictPair (wgt x) (median x)) FL.list
{-# INLINE weightedMedianF #-}
