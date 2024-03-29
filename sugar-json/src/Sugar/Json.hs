{-# LANGUAGE TupleSections, DeriveGeneric, OverloadedStrings, CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Sugar.Json
  ( SugarCube(..)
  , sugarCubeMay
  , writeJsonAsSugarBinary
  , writeJsonAsSugar
  ) where

import Data.Text (Text)
import Data.Map (Map)
import Data.Scientific (floatingOrInteger)

import qualified Data.Map as Map

#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as KeyMap
#else
import qualified Data.HashMap.Strict as KeyMap
#endif

import qualified Data.Serialize as Serialize
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Aeson as Json
import qualified Data.Vector as V
import qualified Data.Text.IO as TIO
import qualified Data.Text as T

import Sugar

-- SugarCube is a refined type of Sugar.
-- This is a useful interface when a Json-like format is easier.
-- Differences:
-- * No notes
-- * No list wrap
-- * Maps are string-key value pairs
data SugarCube
  = SugarCube'Unit
  | SugarCube'Text Text
  | SugarCube'List [SugarCube]
  | SugarCube'Map (Map Text SugarCube)
  deriving (Eq, Show)

class ToSugarCube a where
  toSugarCube :: a -> SugarCube

sugarCubeMay :: Sugar -> Maybe SugarCube
sugarCubeMay (Unit _) = Just SugarCube'Unit
sugarCubeMay (Text t _ _) = Just $ SugarCube'Text t
sugarCubeMay (List xs _ _) = do
  xs' <- mapM sugarCubeMay xs
  return $ SugarCube'List xs'
sugarCubeMay (Map xs _) = do
  xs' <- mapM (\(k,v) -> (,) <$> sugarTextMay k <*> sugarCubeMay v) xs
  return $ SugarCube'Map (Map.fromList xs')

  
writeJsonAsSugarBinary :: FilePath -> FilePath -> IO ()
writeJsonAsSugarBinary src des = do
  bsl <- BL.readFile src
  let value' = Json.decode' bsl :: Maybe Sugar
  case value' of
    Nothing -> putStrLn "Can not decode"
    Just sugar ->  BS.writeFile des $ Serialize.encode sugar

writeJsonAsSugar :: FilePath -> FilePath -> IO ()
writeJsonAsSugar src des = do
  bsl <- BL.readFile src
  let value' = Json.decode' bsl :: Maybe Sugar
  case value' of
    Nothing -> putStrLn "Can not decode"
    Just sugar ->  TIO.writeFile des $ prettyPrintSugar sugar


instance ToSugar SugarCube where
  toSugar SugarCube'Unit = Unit Nothing
  toSugar (SugarCube'Text t) = sanitizeTextToSugar t
  toSugar (SugarCube'List xs) = List (map (toSugarWithWrap Paren) xs) Square Nothing
    where
      -- Alternate nesting between Wrap types
      toSugarWithWrap w c = case c of
        SugarCube'List ys -> List (map (toSugarWithWrap (case w of Square -> Paren; Paren -> Square)) ys) w Nothing
        _ -> toSugar c
  toSugar (SugarCube'Map m) = Map (map (\(k,v) -> (toSugar k, toSugar v)) $ Map.toList m) Nothing

instance ToSugarCube Json.Value where
  toSugarCube Json.Null = SugarCube'Unit
  toSugarCube (Json.Bool b) = SugarCube'Text (if b then "#t" else "#f")
  toSugarCube (Json.String t) = SugarCube'Text t
  toSugarCube (Json.Number n) = SugarCube'Text (showNumber n)
    where
      showNumber s = T.pack $ either show show $ (floatingOrInteger s :: Either Double Integer)
  toSugarCube (Json.Array a) = SugarCube'List $ map toSugarCube (V.toList a)
  toSugarCube (Json.Object o) = SugarCube'Map . Map.fromList . map (\(k,v) -> (keyText k, toSugarCube v)) . KeyMap.toList $ o
    where
#if MIN_VERSION_aeson(2,0,0)
      keyText = AesonKey.toText
#else
      keyText = id
#endif
  
instance Json.FromJSON SugarCube where
  parseJSON v = pure $ toSugarCube v
  
instance Json.FromJSON Sugar where
  parseJSON v = pure . toSugar . toSugarCube $ v
