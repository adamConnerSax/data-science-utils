{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Graphics.VegaLite.ParameterPlot
  ( ParameterEstimate(..)
  , NamedParameterEstimate (..)
  , parameterPlotVsTime
  , parameterPlot
  , parameterPlotMany
  , parameterPlotFlex
  , DateTime (..)
  )
where

import qualified Control.Foldl                 as FL
import qualified Data.Array                    as A
import           Data.Functor.Identity          ( Identity(Identity) )
import           Data.Maybe                     (fromMaybe)
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Graphics.Vega.VegaLite        as GV
import           Graphics.Vega.VegaLite         (DateTime (..))
import qualified Data.List                     as List
import           Text.Printf                    ( printf )

import qualified Statistics.Types              as S


-- | A type to represent the details of a parameter estimate
data ParameterEstimate = ParameterEstimate  { value :: Double, confidence :: (Double, Double) }
data NamedParameterEstimate = NamedParameterEstimate { name :: Text, pEstimate :: ParameterEstimate }

-- | Plot parameters vs. time
parameterPlotVsTime :: (Functor f, Foldable f, A.Ix a)
  => T.Text -- ^ Plot Title
  -> T.Text -- ^ Label of ordinal type
  -> Maybe T.Text -- ^ optional name of estimate value
  -> (a -> Int)
  -> a -- ^ lower bound on time
  -> a -- ^ upper bound on time
  -> f (T.Text, [(a, ParameterEstimate)]) -- ^ data 
  -> GV.VegaLite
parameterPlotVsTime title timeName valNameM timeToInt lower upper orderedParameterValues =
  let valName = fromMaybe "Estimate" valNameM
      toRowData n a (ParameterEstimate e (lo, hi)) =
        [ ("Parameter", GV.Str n)
        , (timeName, GV.Number $ realToFrac $ timeToInt a)
        , (valName, GV.Number e)
        , ("ConfLo", GV.Number lo)
        , ("ConfHi", GV.Number hi)
        ]
      onePFold name = FL.Fold (\l (a, pe) -> (flip GV.dataRow [] $ toRowData name a pe) : l) [] (concat . reverse)  
      dataRowFold = FL.Fold
        (\l (name,a) ->  FL.fold (onePFold name) a : l)
        []
        (GV.dataFromRows [] . concat . reverse)
      dat = FL.fold dataRowFold orderedParameterValues
      xEnc = GV.position GV.X [GV.PName valName
                              , GV.PmType GV.Quantitative
                              ]
      yEnc = GV.position GV.Y [GV.PName timeName
                              , GV.PmType GV.Quantitative
                              , GV.PScale [GV.SDomain $ GV.DNumbers [realToFrac $ timeToInt upper, realToFrac $ timeToInt lower]]
                              ]
      orderEnc = GV.order [GV.OName timeName, GV.OmType GV.Quantitative, GV.OSort [GV.Descending]]
      colorEnc = GV.color [GV.MName "Parameter", GV.MmType GV.Nominal]
      enc = xEnc . yEnc . colorEnc . orderEnc
      filter name = GV.transform . GV.filter (GV.FEqual "Parameter" (GV.Str name))
      spec name = GV.asSpec [(GV.encoding . enc) [], GV.mark GV.Line [], filter name []]
      specs = fmap (spec . fst) $ FL.fold FL.list orderedParameterValues
      configuration = 
        GV.configure
          . GV.configuration (GV.View [GV.ViewWidth 800, GV.ViewHeight 400])
          . GV.configuration (GV.Padding $ GV.PSize 50)
      vl = GV.toVegaLite
        [ GV.title title
        , GV.layer specs
        , dat
        , configuration []
        ]
  in vl

-- | Plot parameters with error bars
--  Flex version handles a foldable of results so we can, e.g., 
--  1. Compare across time or other variable in the data
--  2. Compare different fitting methods for the same data

-- TODO: Fix replicated y-axis ticks.  There since we need the difference between them (trailing "'"s) to provide y-offset
-- to the different results.  Otherwise they would overlap in y.  Maybe fix by switching to layers and only having Y-axis labels
-- on 0th?

-- TODO: Make this typed?  Using a regression result with typed parameters?  Power-to-weight? 



parameterPlot
  :: (Functor f, Foldable f)
  => T.Text
  -> S.CL Double
  -> f NamedParameterEstimate
  -> GV.VegaLite
parameterPlot title cl parameters =
  parameterPlotFlex False id title cl (fmap (\pd -> ("", pd)) parameters)

parameterPlotMany
  :: Foldable f
  => (k -> T.Text)
  -> T.Text
  -> S.CL Double
  -> f (k, NamedParameterEstimate)
  -> GV.VegaLite
parameterPlotMany = parameterPlotFlex True

parameterPlotFlex
  :: Foldable f
  => Bool
  -> (k -> Text)
  -> T.Text
  -> S.CL Double
  -> f (k, NamedParameterEstimate)
  -> GV.VegaLite
parameterPlotFlex haveLegend printKey title cl results
  = let
      toRow m (NamedParameterEstimate n (ParameterEstimate e (lo, hi))) =
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
        , GV.layer [estSpec, confSpec]
        , dat
        , configuration []
        ]
    in
      vl
