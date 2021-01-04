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

weightedMedian :: forall a.(Ord a) => a -> [(Double, a)] -> a
weightedMedian dfltA l =
  let ordered :: [(Double, a)] = L.sortOn snd l
      middleWeight :: Double = FL.fold (FL.premap fst FL.sum) l / 2
      update :: (Double, a) -> (Double, a) -> ((Double, a), ())
      update (wgtSoFar, medianSoFar) (w, x) = ((wgtSoFar + w, newMedian), ()) where
        newMedian = if wgtSoFar <= middleWeight then x else medianSoFar
      -- mapAccumL :: Traversable t => (a -> b -> (a, c)) -> a -> t b -> (a, t c)
      ((_, res), _) = L.mapAccumL update (0, dfltA) ordered
  in res
{-# INLINE weightedMedian #-}

weightedMedianF :: (Num b, Ord b) => (a -> Double) -> (a -> b) -> FL.Fold a b
weightedMedianF wgt median = weightedMedian 0 <$> FL.premap (wgt &&& median) FL.list
{-# INLINE weightedMedianF #-}
