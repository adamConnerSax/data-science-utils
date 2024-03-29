{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Data.MapRow where

import Prelude hiding (fromList, toList)
import qualified Control.Foldl as Foldl
import qualified Data.Map.Strict as Map
import qualified Data.Map.Merge.Strict as Map

import qualified Graphics.Vega.VegaLite as GV

type MapRow a = Map Text a

fromList :: [Text] -> MapRow ()
fromList = Map.fromList . fmap (, ())

withNames :: (Foldable f, Foldable g, Show a) => f Text -> g a -> Either Text (MapRow a)
withNames names values = fmap Map.fromList namedValues
  where
    toList = Foldl.fold Foldl.list
    namedValues =
      if Foldl.fold Foldl.length names == Foldl.fold Foldl.length values
        then Right $ zip (toList names) (toList values)
        else
          Left
            ( "Names ("
                <> show (toList names)
                <> ") and values ("
                <> show (toList values)
                <> ") have different lengths in nameRow."
            )

changeColNameInRow :: Text -> Text -> MapRow a -> Either Text (MapRow a)
changeColNameInRow old new row = case Map.lookup old row of
  Nothing -> Left $ "\"" <> old <> "\" not found in row with colNames " <> show (Map.keys row)
  Just a -> Right $ Map.insert new a $ Map.delete old row

changeColName :: Traversable f => Text -> Text -> f (MapRow a) -> Either Text (f (MapRow a))
changeColName old new = traverse (changeColNameInRow old new)

keyedMapRows :: (Foldable f, Ord k) => (a -> k) -> Text -> f (MapRow a) -> Either Text (Map k (MapRow a))
keyedMapRows toKey colName = Foldl.foldM (Foldl.premapM getKeyValueM $ Foldl.generalize Foldl.map)  where
  getKeyValueM mr = do
    rowA <- maybe (Left $ "Failed to find key column in keyedMapRows") Right $ Map.lookup colName mr
    return (toKey rowA, mr)

-- result is only rows which have cols in both collections
joinKeyedMapRows :: Ord k => Map k (MapRow a) -> Map k (MapRow a) -> Map k (MapRow a)
joinKeyedMapRows = Map.merge Map.dropMissing Map.dropMissing (Map.zipWithMaybeMatched f)  where
  f _ mrA mrB = Just $ mrA <> mrB


type VLDataField = (GV.FieldName, GV.DataValue)

toVLDataFields :: (a -> GV.DataValue) -> MapRow a -> [VLDataField]
toVLDataFields vlDataVal = Map.toList . fmap vlDataVal

toVLData :: (Functor f, Foldable f) => (MapRow a -> [(GV.FieldName, GV.DataValue)]) -> [GV.Format] -> f (MapRow a) -> GV.Data
toVLData vlFields fmt mapRows =
  ( GV.dataFromRows fmt
      . Foldl.fold dataRowF (
            fmap vlFields mapRows)
  )
    []
  where
    dataRowF = Foldl.Fold (\rows tupleList -> rows . GV.dataRow tupleList) id id

dataValueText :: GV.DataValue -> Text
dataValueText (GV.Boolean b) = "Boolean: " <> show b
dataValueText (GV.DateTime _) = "DateTime"
dataValueText (GV.Number n) = "Number: " <> show n
dataValueText (GV.Str s) = "Str: " <> s
dataValueText GV.NullValue = "NullValue"
