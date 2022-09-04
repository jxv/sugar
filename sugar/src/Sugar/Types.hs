{-# LANGUAGE TupleSections, DeriveGeneric, OverloadedStrings, CPP #-}
module Sugar.Types
  ( Sugar(..)
  , Wrap(..)
  , Note
  , readSugarMay
  , sugarTextMay
  , sugarMapAsIxMap
  , reservedChars
  , FromSugar(..)
  , ToSugar(..)
  ) where

import Data.Text (Text)
import Data.Maybe (mapMaybe)
import Data.Text.Conversions (ToText(..), fromText, unUTF8, decodeConvertText, UTF8(..))
import Data.String (IsString(..))
import Data.Word (Word8,Word16,Word32,Word64)
import Data.Int (Int8,Int16,Int32,Int64)
import GHC.Generics (Generic)
import Safe (readMay)

import qualified Data.Map as Map
import qualified Data.Serialize as Serialize
import qualified Data.Store as Store ()
import qualified Data.ByteString as BS
import qualified Data.Text as T

---

data Sugar
  = Unit Note
  | Text Text Note
  | List [Sugar] Wrap Note
  | Map [(Sugar,Sugar)] Note
  deriving (Eq, Show, Generic)

data Wrap
  = Square
  | Paren
  deriving (Eq, Show, Generic)

type Note = Maybe [Sugar]

--

sugarTextMay :: Sugar -> Maybe Text
sugarTextMay (Text t _) = Just t
sugarTextMay _ = Nothing

readSugarMay :: Read a => Sugar -> Maybe a
readSugarMay (Text t _) = readMay $ T.unpack t
readSugarMay _ = Nothing

sugarMapAsIxMap :: [(Sugar,Sugar)] -> Map.Map (Int, Sugar) Sugar
sugarMapAsIxMap = Map.fromList . zipWith (\i (k,v) -> ((i,k),v)) [0..]

sugarShow :: Show a => a -> Sugar
sugarShow s = Text (T.pack $ show s) Nothing

reservedChars :: [Char]
reservedChars = ['\"','[',']','<','>','(',')','{','}',';',',',':']

--

instance Ord Sugar where
  compare (Unit x) (Unit y) = compare x y
  compare Unit{} _ = GT
  compare _ Unit{} = LT
  compare (Text x0 x1) (Text y0 y1) = compare x0 y0 `mappend` compare x1 y1
  compare Text{} _ = GT
  compare _ Text{} = LT
  compare (List x0 _ x1) (List y0 _ y1) = compare x0 y0 `mappend` compare x1 y1
  compare List{} _ = GT
  compare _ List{} = LT
  compare (Map x0 x1) (Map y0 y1) = compare x0 y0 `mappend` compare x1 y1

instance Serialize.Serialize Sugar where
  get = do
    tag <- Serialize.getWord8
    go tag
    where
      go :: Word8 -> Serialize.Get Sugar
      go 0 = Unit <$> Serialize.get
      go 1 = Text <$> getSerializedText <*> Serialize.get
      go 2 = List <$> Serialize.get <*> Serialize.get <*> Serialize.get
      go 3 = Map <$> Serialize.get <*> Serialize.get
      go _ = fail "No matching Sugar value"

      getSerializedText :: Serialize.Get Text
      getSerializedText = do
        txt <- (decodeConvertText . UTF8) <$> (Serialize.get :: Serialize.Get BS.ByteString)
        maybe (fail "Cannot deserialize text as UTF8") pure txt

  put (Unit note) = do
    Serialize.put (0 :: Word8)
    Serialize.put note
  put (Text txt note) = do
    Serialize.put (1 :: Word8)
    Serialize.put (unUTF8 $ fromText txt :: BS.ByteString)
    Serialize.put note
  put (List xs w note) = do
    Serialize.put (2 :: Word8)
    Serialize.put xs
    Serialize.put w
    Serialize.put note
  put (Map m note) = do
    Serialize.put (3 :: Word8)
    Serialize.put m
    Serialize.put note

instance Serialize.Serialize Wrap where

instance IsString Sugar where
  fromString str = Text (toText str) Nothing


class FromSugar a where
  parseSugar :: Sugar -> Maybe a

instance FromSugar a => FromSugar [a] where
  parseSugar (List xs _ _) = mapM parseSugar xs
  parseSugar _ = Nothing

instance FromSugar a => FromSugar (Maybe a) where
  parseSugar (Unit _) = Just Nothing
  parseSugar s = (return . Just) =<< parseSugar s

instance (FromSugar a, Ord a, FromSugar b) => FromSugar (Map.Map a b) where
  parseSugar (Map m _) = Just $ Map.fromList $
    mapMaybe
      (\(s,v) -> (,) <$> parseSugar s <*> parseSugar v)
      m
  parseSugar _ = Nothing

instance FromSugar Text where
  parseSugar (Text t _) = Just t
  parseSugar _ = Nothing

instance FromSugar Bool where
  parseSugar (Text "#t" _) = Just True
  parseSugar (Text "#f" _) = Just False
  parseSugar _ = Nothing

instance FromSugar Integer where parseSugar = readSugarMay
instance FromSugar Int where parseSugar = readSugarMay
instance FromSugar Int8 where parseSugar = readSugarMay
instance FromSugar Int16 where parseSugar = readSugarMay
instance FromSugar Int32 where parseSugar = readSugarMay
instance FromSugar Int64 where parseSugar = readSugarMay
instance FromSugar Word where parseSugar = readSugarMay
instance FromSugar Word8 where parseSugar = readSugarMay
instance FromSugar Word16 where parseSugar = readSugarMay
instance FromSugar Word32 where parseSugar = readSugarMay
instance FromSugar Word64 where parseSugar = readSugarMay
instance FromSugar Float where parseSugar = readSugarMay
instance FromSugar Double where parseSugar = readSugarMay


class ToSugar a where
  toSugar :: a -> Sugar

instance ToSugar () where
  toSugar () = Unit Nothing

instance ToSugar Text where
  toSugar t = Text t Nothing

-- TODO: Will conflict with a String instance (aka [Char])
instance ToSugar a => ToSugar [a] where
  toSugar xs = List (map toSugar xs) Square Nothing

instance ToSugar a => ToSugar (Maybe a) where
  toSugar Nothing = Unit Nothing
  toSugar (Just a) = toSugar a

instance (ToSugar a, ToSugar b) => ToSugar (Map.Map a b) where
  toSugar m = Map (map (\(k,v) -> (toSugar k, toSugar v)) $ Map.toList m) Nothing

instance (ToSugar a, ToSugar b) => ToSugar (a,b) where
  toSugar (a,b) = List [toSugar a, toSugar b] Paren Nothing

instance (ToSugar a, ToSugar b, ToSugar c) => ToSugar (a,b,c) where
  toSugar (a,b,c) = List [toSugar a, toSugar b, toSugar c] Paren Nothing

instance ToSugar Bool where
  toSugar s = toSugar (if s then "#t" else "#f" :: Text)

instance ToSugar Integer where toSugar = sugarShow
instance ToSugar Int where toSugar = sugarShow
instance ToSugar Int8 where toSugar = sugarShow
instance ToSugar Int16 where toSugar = sugarShow
instance ToSugar Int32 where toSugar = sugarShow
instance ToSugar Int64 where toSugar = sugarShow
instance ToSugar Word where toSugar = sugarShow
instance ToSugar Word8 where toSugar = sugarShow
instance ToSugar Word16 where toSugar = sugarShow
instance ToSugar Word32 where toSugar = sugarShow
instance ToSugar Word64 where toSugar = sugarShow
instance ToSugar Float where toSugar = sugarShow
instance ToSugar Double where toSugar = sugarShow
