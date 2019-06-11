{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
module Visualization.VegaLite.StackedArea
  (
    -- * Visualizations
    stackedAreaVsTime
    -- * Configuration Re-exports
  , ViewConfig
  )
where

import qualified Visualization.VegaLite.Common as VC
import           Visualization.VegaLite.Common  ( ViewConfig(..) )

import qualified Control.Foldl                 as FL
import           Control.Monad                  ( join )
import qualified Data.Array                    as A
import           Data.Functor.Identity          ( Identity(Identity) )
import           Data.Maybe                     ( fromMaybe )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Graphics.Vega.VegaLite        as GV
import           Graphics.Vega.VegaLite         ( DateTime(..) )
import qualified Data.List                     as List
import           Text.Printf                    ( printf )

import qualified Statistics.Types              as S


data StackedArea k = StackedArea { groupField :: k, timeField :: k, amountField :: k}
stackedAreaVsTimeGOG :: (Traversable f, A.Ix k)
  => T.Text -- ^ Title
  -> StackedArea k -- ^ Stacked Area field indices
  -> VC.DataRows f k -- ^data
  -> GV.TimeUnit -- ^ time resolution for X encoding 
  -> VC.ViewConfig -- sizing information 
  -> GV.VegaLite
stackedAreaVsTimeGOG title (StackedArea kGroup kTime kAmount) dr@(VC.DataRows fi _) tu vc =
  let dat = VC.toHvegaData dr
      xEnc = GV.position GV.X [ GV.PName (VC.labelAt fi kTime), GV.PmType GV.Temporal, GV.PTimeUnit tu]
      yEnc = GV.position GV.Y [ GV.PName (VC.labelAt fi kAmount), GV.PmType GV.Quantitative]
      colorEnc = GV.color [GV.MName (VC.labelAt fi kGroup), GV.MmType GV.Nominal]
      enc = xEnc . yEnc . colorEnc
      specs =
        [ GV.asSpec
            [ (GV.encoding . enc) []
            , GV.mark GV.Area [GV.MInterpolate GV.Monotone]
            ]
        ]          
      configuration = GV.configure . VC.viewConfigAsHvega vc
  in GV.toVegaLite [GV.title title, GV.layer specs, dat, configuration []]

        
stackedAreaVsTime
  :: (Traversable f, Real b)
  => T.Text -- ^ Title
  -> T.Text -- ^ Label for Text field
  -> T.Text -- ^ Label for temporal type a
  -> T.Text -- ^ Label for quantitative type b
  -> VC.TimeEncoding a -- ^ encoding for time type
  -> VC.ViewConfig
  -> f (T.Text, [(a, b)]) -- ^ data, already in order?
  -> GV.VegaLite
stackedAreaVsTime title groupLabel timeLabel dataLabel timeEnc vc labeledTemporalData
  = let
      toRowData n a b =
        [ (groupLabel, GV.Str n)
        , (timeLabel , GV.Str $ VC.toStr timeEnc a)
        , (dataLabel , GV.Number $ realToFrac b)
        ]
      onePFold name = FL.Fold
        (\l (a, b) -> (flip GV.dataRow [] $ toRowData name a b) : l)
        []
        (concat . reverse)
      dataRowFold = FL.Fold
        (\l (name, abs) -> FL.fold (onePFold name) abs : l)
        []
        ( GV.dataFromRows
            [GV.Parse [(timeLabel, GV.FoDate $ VC.timeFormat timeEnc)]]
        . concat
        . reverse
        )
      dat  = FL.fold dataRowFold labeledTemporalData
      xEnc = GV.position
        GV.X
        [ GV.PName timeLabel
        , GV.PmType GV.Temporal
        , GV.PTimeUnit (VC.timeUnit timeEnc)
        ]
      yEnc = GV.position GV.Y [GV.PName dataLabel, GV.PmType GV.Quantitative]
      colorEnc = GV.color [GV.MName groupLabel, GV.MmType GV.Nominal]
--      orderEnc = GV.order [GV.OName timeLabel, GV.OmType GV.Temporal]
      enc = xEnc . yEnc . colorEnc
      filter name =
        GV.transform . GV.filter (GV.FEqual groupLabel (GV.Str name))
      areaSpec name = GV.asSpec
        [ (GV.encoding . enc) []
        , GV.mark GV.Area [GV.MInterpolate GV.Monotone]
        , filter name []
        ]
--      specs = fmap (areaSpec . fst) $ FL.fold FL.list labeledTemporalData
      specs =
        [ GV.asSpec
            [ (GV.encoding . enc) []
            , GV.mark GV.Area [GV.MInterpolate GV.Monotone]
            ]
        ]
      configuration = GV.configure . VC.viewConfigAsHvega vc
      vl =
        GV.toVegaLite [GV.title title, GV.layer specs, dat, configuration []]
    in
      vl
