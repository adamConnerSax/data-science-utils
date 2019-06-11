{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
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
    -- * GOG 
  , FieldType(..)
  , FieldValue(..)
  , FieldIndex(..)
  , labelAt
  , indexFor
  , changeLabel
  , DataRows(..)
  , checkLabels
  , buildDataRows
  , toHvegaData
  , strLoader
  , countLoader
  , numLoader
  , dateTimeLoader
  , intYearLoader
  , boolLoader
    -- * Re-exports
--  , TimeUnit(..)
  )
where

import           Control.Arrow                  ( second )
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
timeField (TimeEncoding toStrF _ _) x = GV.Str $ toStrF x

-- General types to represent visualizable row data

-- | Enum to flag which type is being held
data FieldType = StrField | CountField | NumberField | DateTimeField | IntYearField | BooleanField deriving (Show, Enum, Bounded, Ord, Eq)

-- | type to store a single value, either text, numerical or temporal
data FieldValue where
  Str :: Text -> FieldValue
  Count :: Int -> FieldValue
  Number :: Double -> FieldValue
  DateTime :: Time.LocalTime -> FieldValue
  IntYear :: Int -> FieldValue
  Boolean :: Bool -> FieldValue deriving (Eq, Show)

{-
itemFieldType :: FieldValue -> FieldType
itemFieldType (Str      _) = StrField
itemFieldType (Count    _) = CountField
itemFieldType (Number   _) = NumberField
itemFieldType (DateTime _) = DateTimeField
itemFieldType (IntYear  _) = IntYearField
itemFieldType (Boolean  _) = BooleanField
-}
-- | type to represent the String version of column names and map to the index
data FieldIndex k where
  FieldIndex :: A.Ix k => M.Map T.Text k ->  A.Array k (T.Text, FieldType) -> FieldIndex k

labelAt :: A.Ix k => FieldIndex k -> k -> T.Text
labelAt (FieldIndex _ fa) k = fst $ fa A.! k

fieldTypeAt :: A.Ix k => FieldIndex k -> k -> FieldType
fieldTypeAt (FieldIndex _ fa) k = snd $ fa A.! k

indexFor :: A.Ix k => FieldIndex k -> T.Text -> Maybe k
indexFor (FieldIndex indexMap _) = flip M.lookup indexMap

changeLabel :: T.Text -> T.Text -> FieldIndex k -> Maybe (FieldIndex k)
changeLabel old new fi@(FieldIndex indexMap labelTypeArray) = do
  index <- indexFor fi old
  let newIndexMap    = M.insert new index (M.delete old indexMap)
      (_, fieldType) = labelTypeArray A.! index
      newLTA         = labelTypeArray A.// [(index, (new, fieldType))]
  return $ FieldIndex newIndexMap newLTA

fieldIndexFromLabeledTypes :: [(T.Text, FieldType)] -> FieldIndex Int
fieldIndexFromLabeledTypes labelsAndTypes =
  let labelTypeArray =
        A.listArray (0, length labelsAndTypes - 1) labelsAndTypes
      indexMap =
        M.fromList $ fmap (\(k, (l, _)) -> (l, k)) $ A.assocs labelTypeArray
  in  FieldIndex indexMap labelTypeArray

data DataRows f k where
  DataRows :: (A.Ix k, Traversable f) => FieldIndex k -> f (A.Array k FieldValue) -> DataRows f k

checkLabels :: DataRows f k -> [(T.Text, [FieldType])] -> Bool
checkLabels (DataRows fi _) =
  let f :: (T.Text, [FieldType]) -> Maybe ()
      f (t, fts) = do
        index <- indexFor fi t
        if (elem (fieldTypeAt fi index) fts) then Just () else Nothing
  in  maybe False (const True) . traverse f

-- don't expose this constructor!!
data FieldLoader a = FieldLoader { flType :: FieldType, fLoader :: a -> FieldValue }

strLoader :: (a -> T.Text) -> FieldLoader a
strLoader f = FieldLoader StrField (Str . f)

countLoader :: (a -> Int) -> FieldLoader a
countLoader f = FieldLoader NumberField (Count . f)

numLoader :: (a -> Double) -> FieldLoader a
numLoader f = FieldLoader NumberField (Number . f)

dateTimeLoader :: (a -> Time.LocalTime) -> FieldLoader a
dateTimeLoader f = FieldLoader DateTimeField (DateTime . f)

intYearLoader :: (a -> Int) -> FieldLoader a
intYearLoader f = FieldLoader IntYearField (IntYear . f)

boolLoader :: (a -> Bool) -> FieldLoader a
boolLoader f = FieldLoader BooleanField (Boolean . f)

buildDataRows
  :: Traversable f => [(T.Text, FieldLoader a)] -> f a -> DataRows f Int
buildDataRows labeledLoaders dat =
  let fieldIndex =
        fieldIndexFromLabeledTypes $ fmap (second flType) labeledLoaders
      loaderArray = A.listArray (0, length labeledLoaders - 1)
        $ fmap (fLoader . snd) labeledLoaders
      rows = fmap (\a -> fmap ($ a) loaderArray) dat
  in  DataRows fieldIndex rows

-- Hvega specific

toHvegaDataValue :: FieldValue -> GV.DataValue
toHvegaDataValue (Str      x) = GV.Str x
toHvegaDataValue (Count    x) = GV.Number $ realToFrac x
toHvegaDataValue (Number   x) = GV.Number x
toHvegaDataValue (Boolean  x) = GV.Boolean x
toHvegaDataValue (IntYear  x) = GV.Str $ T.pack $ show x
toHvegaDataValue (DateTime x) = GV.Str $ T.pack $ show x -- yyyy-mm-dd hh:mm:ss  

-- add parsing info when required
hvegaDataType :: FieldType -> Maybe GV.DataType
hvegaDataType IntYearField  = Just $ GV.FoDate "%Y"
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



