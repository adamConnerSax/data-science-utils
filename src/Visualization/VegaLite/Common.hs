{-# LANGUAGE GADTs                 #-}
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
--  , TimeUnit(..)
  )
where

import           Control.Arrow                  ( (***)
                                                , (&&&)
                                                )
import qualified Control.Foldl                 as FL
import qualified Data.Array                    as A
import qualified Data.Map                      as M
import           Data.Maybe                     ( catMaybes )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Time                     as Time
import qualified Graphics.Vega.VegaLite        as GV
--import           Graphics.Vega.VegaLite         ( TimeUnit(..) )

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

-- General types to represent visualizable row data

-- | type to store a single value, either text, numerical or temporal
data FieldType = StrField | NumberField | DateTimeField | YearField | BooleanField

data FieldValue where
  Str :: Text -> FieldValue
  Number :: Real a => a -> FieldValue
  DateTime :: Time.LocalTime -> FieldValue
  Year :: Int -> FieldValue
  Boolean :: Bool -> FieldValue

itemFieldType :: FieldValue -> FieldType
itemFieldType (Str      _) = StrField
itemFieldType (Number   _) = NumberField
itemFieldType (DateTime _) = DateTimeField
itemFieldType (Year     _) = YearField
itemFieldType (Boolean  _) = BooleanField

-- | type to represent the String version of column names and map to the index
data FieldIndex k where
  FieldIndex :: A.Ix k => M.Map T.Text k ->  A.Array k (T.Text, FieldType) -> FieldIndex k

data DataRows f k where
  DataRows :: (A.Ix k, Traversable f) => FieldIndex k -> f (A.Array k FieldValue) -> DataRows f k

-- Hvega specific

toHvegaDataValue :: FieldValue -> GV.DataValue
toHvegaDataValue (Str      x) = GV.Str x
toHvegaDataValue (Number   x) = GV.Number $ realToFrac x
toHvegaDataValue (Boolean  x) = GV.Boolean x
toHvegaDataValue (Year     x) = GV.Str $ T.pack $ show x
toHvegaDataValue (DateTime x) = GV.Str $ T.pack $ show x -- yyyy-mm-dd hh:mm:ss  

-- add parsing info when required
hvegaDataType :: FieldType -> Maybe GV.DataType
hvegaDataType YearField     = Just $ GV.FoDate "%Y"
hvegaDataType DateTimeField = Just $ GV.FoDate "%Y-%mm-%dd %H:%M:%S"
hvegaDataType _             = Nothing

hvegaParseList :: FieldIndex k -> [(T.Text, GV.DataType)]
hvegaParseList (FieldIndex _ labelTypeArray) =
  let f (a, mb) = (,) <$> pure a <*> mb
  in  catMaybes $ fmap (f . (\(_, (l, ft)) -> (l, hvegaDataType ft))) $ A.assocs
        labelTypeArray

toHvegaDataRow :: FieldIndex k -> A.Array k FieldValue -> [GV.DataRow]
toHvegaDataRow (FieldIndex _ labelTypeArray) =
  flip GV.dataRow []
    . fmap (\(k, fv) -> (fst $ labelTypeArray A.! k, toHvegaDataValue fv))
    . A.assocs

toHvegaData :: DataRows f k -> GV.Data
toHvegaData (DataRows fi rows) =
  GV.dataFromRows [GV.Parse $ hvegaParseList fi]
    $ concat
    $ FL.fold FL.list
    $ fmap (toHvegaDataRow fi) rows



