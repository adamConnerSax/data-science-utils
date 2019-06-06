{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
module Visualization.VegaLite.StackedArea
  ()
where

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





stackedAreaVsTime
  :: Traversable f
  => T.Text
  -> TimeEncoding a
  -> f (T.Text, [(a, b)])
  -> GV.VegaLite
stackedAreaVsTime

-- | A type to represent the details of a parameter estimate
data ParameterEstimate = ParameterEstimate  { value :: Double, confidence :: (Double, Double) }
data NamedParameterEstimate = NamedParameterEstimate { name :: Text, pEstimate :: ParameterEstimate }
data YScaling = Default | DataMinMax deriving (Eq)
data TimeEncoding a = TimeEncoding { toStr :: a -> Text, timeFormat :: Text, timeUnit :: GV.TimeUnit }

-- helpers for time encoding
-- this should be a separate module with support for a variety of things.
intYear :: TimeEncoding Int
intYear = TimeEncoding (T.pack . show) "%Y" GV.Year

-- | Plot parameters vs. time
parameterPlotVsTime
  :: (Traversable f, A.Ix a)
  => T.Text -- ^ Plot Title
  -> T.Text -- ^ Label of ordinal type
  -> Maybe T.Text -- ^ optional name of parameter
  -> Maybe T.Text -- ^ optional name of estimate value
  -> YScaling
  -> TimeEncoding a
  -> f (T.Text, [(a, ParameterEstimate)]) -- ^ data 
  -> GV.VegaLite
parameterPlotVsTime title timeName parameterNameM valNameM yScaling (TimeEncoding toDTs tf tu) orderedParameterValues
  = let
      valName       = fromMaybe "Estimate" valNameM
      parameterName = fromMaybe "Parameter" parameterNameM
      minYM         = join $ fmap (FL.fold FL.minimum) $ traverse
        (FL.fold (FL.premap (value . snd) FL.minimum) . snd)
        orderedParameterValues
      maxYM = join $ fmap (FL.fold FL.maximum) $ traverse
        (FL.fold (FL.premap (value . snd) FL.maximum) . snd)
        orderedParameterValues
      yScale = case (,) <$> minYM <*> maxYM of
        Nothing       -> []
        Just (lo, hi) -> if (yScaling == DataMinMax)
          then [GV.PScale [GV.SDomain $ GV.DNumbers [lo, hi]]]
          else []
      toRowData n a (ParameterEstimate e (lo, hi)) =
        [ ( parameterName
          , GV.Str n
          )
--        , (timeName     , GV.Number $ realToFrac $ timeToInt a)
        , (timeName, GV.Str $ toDTs a)
        , (valName , GV.Number e)
        , ("ConfLo", GV.Number lo)
        , ("ConfHi", GV.Number hi)
        ]
      onePFold name = FL.Fold
        (\l (a, pe) -> (flip GV.dataRow [] $ toRowData name a pe) : l)
        []
        (concat . reverse)
      dataRowFold = FL.Fold
        (\l (name, a) -> FL.fold (onePFold name) a : l)
        []
        ( GV.dataFromRows [GV.Parse [(timeName, GV.FoDate tf)]]
        . concat
        . reverse
        )
      dat = FL.fold dataRowFold orderedParameterValues
      yEnc =
        GV.position GV.Y
          $  [GV.PName valName, GV.PmType GV.Quantitative]
          ++ yScale
      xEnc = GV.position
        GV.X
        [GV.PName timeName, GV.PmType GV.Temporal, GV.PTimeUnit tu]
      orderEnc = GV.order [GV.OName timeName, GV.OmType GV.Temporal]
      colorEnc = GV.color [GV.MName parameterName, GV.MmType GV.Nominal]
      enc      = xEnc . yEnc . colorEnc -- . orderEnc
      filter name =
        GV.transform . GV.filter (GV.FEqual parameterName (GV.Str name))
      lSpec name = GV.asSpec
        [ (GV.encoding . enc) []
        , GV.mark GV.Line [GV.MInterpolate GV.Monotone]
        , filter name []
        ]
      mSpec name =
        GV.asSpec [(GV.encoding . enc) [], GV.mark GV.Point [], filter name []]
      specs = concat $ fmap (\(x, _) -> [lSpec x, mSpec x]) $ FL.fold
        FL.list
        orderedParameterValues
      configuration =
        GV.configure
          . GV.configuration (GV.View [GV.ViewWidth 800, GV.ViewHeight 400])
          . GV.configuration (GV.Padding $ GV.PSize 50)
      vl =
        GV.toVegaLite [GV.title title, GV.layer specs, dat, configuration []]
    in
      vl

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
        [GV.title title, GV.layer [estSpec, confSpec], dat, configuration []]
    in
      vl
