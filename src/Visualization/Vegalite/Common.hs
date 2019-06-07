{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Visualization.VegaLite.Common
  (
    -- * Helper Types    
    TimeEncoding(..)
  , Scaling(..)
  , ViewConfig(..)
    -- * helpers
  , intYear
  , timeField
  , viewConfigAsHvega
    -- * Re-exports
  , TimeUnit(..)
  )
where

import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Graphics.Vega.VegaLite        as GV
import           Graphics.Vega.VegaLite         ( TimeUnit(..) )

data Scaling = Default | DataMinMax deriving (Eq)

data ViewConfig = ViewConfig { vcWidth :: Double, vcHeight :: Double, vcPadding :: Double }

viewConfigAsHvega :: ViewConfig -> GV.BuildLabelledSpecs
viewConfigAsHvega (ViewConfig w h p) =
  GV.configuration (GV.View [GV.ViewWidth w, GV.ViewHeight h])
    . GV.configuration (GV.Padding $ GV.PSize p)

data TimeEncoding a = TimeEncoding { toStr :: a -> Text, timeFormat :: Text, timeUnit :: GV.TimeUnit }

-- helpers for time encoding
intYear :: TimeEncoding Int
intYear = TimeEncoding (T.pack . show) "%Y" GV.Year

timeField :: TimeEncoding a -> a -> GV.DataValue
timeField (TimeEncoding toStr _ _) x = GV.Str $ toStr x

