{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Visualization.VegaLite.Common
  (
    -- * Helper Types    
    TimeEncoding(..)
  , Scaling(..)
    -- * helpers
  , intYear
  , timeField
    -- * Re-exports
  , TimeUnit(..)
  )
where

import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Graphics.Vega.VegaLite        as GV
import           Graphics.Vega.VegaLite         ( TimeUnit(..) )

data Scaling = Default | DataMinMax deriving (Eq)

data TimeEncoding a = TimeEncoding { toStr :: a -> Text, timeFormat :: Text, timeUnit :: GV.TimeUnit }

-- helpers for time encoding
intYear :: TimeEncoding Int
intYear = TimeEncoding (T.pack . show) "%Y" GV.Year

timeField :: TimeEncoding a -> a -> GV.DataValue
timeField (TimeEncoding toStr _ _) x = GV.Str $ toStr x

