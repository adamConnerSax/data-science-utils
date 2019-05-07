{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Graphics.VegaLite.ParameterPlot
  ( ParameterDetails(..)
  , parameterPlot
  , parameterPlotMany
  , parameterPlotFlex
  )
where

import qualified Control.Foldl                 as FL
import           Data.Functor.Identity          ( Identity(Identity) )
--import           Data.Maybe                     ( fromMaybe )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Graphics.Vega.VegaLite        as GV
import qualified Data.List                     as List
import           Text.Printf                    ( printf )

import qualified Statistics.Types              as S

-- | Plot parameters with error bars
-- | Flex version handles a foldable of results so we can, e.g., 
-- | 1. Compare across time or other variable in the data
-- | 2. Compare different fitting methods for the same data

-- TODO: Fix replicated y-axis ticks.  There since we need the difference between them (trailing "'"s) to provide y-offset
-- to the different results.  Otherwise they would overlap in y.  Maybe fix by switching to layers and only having Y-axis labels
-- on 0th?

-- TODO: Make this typed?  Using a regression result with typed parameters?  Power-to-weight? 

data ParameterDetails = ParameterDetails  { name :: Text, value :: Double, confidence :: (Double, Double) }

parameterPlot
  :: (Functor f, Foldable f)
  => T.Text
  -> S.CL Double
  -> f ParameterDetails
  -> GV.VegaLite
parameterPlot title cl parameters =
  parameterPlotFlex False id title cl (fmap (\pd -> ("", pd)) parameters)

parameterPlotMany
  :: Foldable f
  => (k -> T.Text)
  -> T.Text
  -> S.CL Double
  -> f (k, ParameterDetails)
  -> GV.VegaLite
parameterPlotMany = parameterPlotFlex True

parameterPlotFlex
  :: Foldable f
  => Bool
  -> (k -> Text)
  -> T.Text
  -> S.CL Double
  -> f (k, ParameterDetails)
  -> GV.VegaLite
parameterPlotFlex haveLegend printKey title cl results
  = let
      toRow m (ParameterDetails n e (lo, hi)) =
        [ ("Parameter", GV.Str n)
        , ("Estimate" , GV.Number e)
        , ("ConfLo"   , GV.Number lo)
        , ("ConfHi"   , GV.Number hi)
        ]
      addKey k l = ("Key", GV.Str $ printKey k) : l
      dataRowFold = FL.Fold
        (\(l, n) (k, pDetails) ->
          ((flip GV.dataRow [] . addKey k . toRow n $ pDetails) : l, n + 1)
        )
        ([], 0)
        (GV.dataFromRows [] . concat . reverse . fst)
      dat = FL.fold dataRowFold results
      xLabel =
        "Estimate (with "
          <> (T.pack $ printf "%2.0f" (100 * S.confidenceLevel cl))
          <> "% confidence error bars)"
      estimateXEnc = GV.position
        GV.X
        [ GV.PName "Estimate"
        , GV.PmType GV.Quantitative
        , GV.PAxis [GV.AxTitle xLabel]
        ]
      estimateYEnc =
        GV.position GV.Y [GV.PName "Parameter", GV.PmType GV.Ordinal]
      handleLegend l = if haveLegend then l else (GV.MLegend []) : l
      estimateColorEnc =
        GV.color $ handleLegend $ [GV.MName "Key", GV.MmType GV.Nominal]
      estimateEnc  = estimateXEnc . estimateYEnc . estimateColorEnc
--      estLoCalc   = "datum.Estimate - datum.Confidence/2"
--      estHiCalc   = "datum.Estimate + datum.Confidence/2"
--      calcEstConf =
--        GV.calculateAs estLoCalc "estLo" . GV.calculateAs estHiCalc "estHi"
      estConfLoEnc = GV.position
        GV.X
        [GV.PName "ConfLo", GV.PmType GV.Quantitative, GV.PAxis [GV.AxTitle ""]]
      estConfHiEnc = GV.position
        GV.X2
        [GV.PName "ConfHi", GV.PmType GV.Quantitative, GV.PAxis [GV.AxTitle ""]]
      estConfEnc =
        estConfLoEnc . estConfHiEnc . estimateYEnc . estimateColorEnc
      estSpec  = GV.asSpec [(GV.encoding . estimateEnc) [], GV.mark GV.Point []]
      confSpec = GV.asSpec [(GV.encoding . estConfEnc) [], GV.mark GV.Rule []]
      configuration =
        GV.configure
          . GV.configuration (GV.View [GV.ViewWidth 800, GV.ViewHeight 400])
          . GV.configuration (GV.Padding $ GV.PSize 50)
      vl = GV.toVegaLite
        [ GV.title title
--        , (GV.transform . calcEstConf) []
        , GV.layer [estSpec, confSpec]
        , dat
        , configuration []
        ]
    in
      vl
