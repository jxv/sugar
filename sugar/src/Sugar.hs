{-# LANGUAGE TupleSections, DeriveGeneric, OverloadedStrings, CPP #-}
module Sugar
  ( Sugar(..)
  , Wrap(..)
  , Note
  , FromSugar(..)
  , readSugarMay
  , sugarMapAsIxMap
  , ToSugar(..)
  , sugarTextMay
  , readSugarFromFile
  , readSugarListFromFile
  , parseSugarFromText
  , parseSugarListFromText
  , prettyPrintSugarIO
  , prettyPrintSugar
  ) where

import Control.Applicative (Alternative(..))
import Data.Void (Void)
import Data.Text (Text)
import Data.Map (Map)
import Data.Maybe (isNothing, mapMaybe)
import Data.Text.Conversions (ToText(..), fromText, unUTF8, decodeConvertText, UTF8(..))
import Data.String (IsString(..))
import Data.Word (Word8,Word16,Word32,Word64)
import Data.Int (Int8,Int16,Int32,Int64)
import Data.Char (isSeparator)
import GHC.Generics (Generic)
import Safe (readMay)

import qualified Data.Map as Map
import qualified Data.Serialize as Serialize
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L

---

data Sugar
  = Sugar'Unit Note
  | Sugar'Text Text Note
  | Sugar'List [Sugar] Wrap Note
  | Sugar'Map [(Sugar,Sugar)] Note
  deriving (Eq, Show, Generic)
  
data Wrap
  = Wrap'Square
  | Wrap'Paren
  deriving (Eq, Show, Generic)
  
type Note = Maybe Sugar
  
--

instance Ord Sugar where
  compare (Sugar'Unit x) (Sugar'Unit y) = compare x y
  compare Sugar'Unit{} _ = GT
  compare _ Sugar'Unit{} = LT
  compare (Sugar'Text x0 x1) (Sugar'Text y0 y1) = compare x0 y0 `mappend` compare x1 y1
  compare Sugar'Text{} _ = GT
  compare _ Sugar'Text{} = LT
  compare (Sugar'List x0 _ x1) (Sugar'List y0 _ y1) = compare x0 y0 `mappend` compare x1 y1
  compare Sugar'List{} _ = GT
  compare _ Sugar'List{} = LT
  compare (Sugar'Map x0 x1) (Sugar'Map y0 y1) = compare x0 y0 `mappend` compare x1 y1

instance Serialize.Serialize Sugar where
  get = do
    tag <- Serialize.getWord8
    go tag
    where
      go :: Word8 -> Serialize.Get Sugar
      go 0 = Sugar'Unit <$> Serialize.get
      go 1 = Sugar'Text <$> getSerializedText <*> Serialize.get
      go 2 = Sugar'List <$> Serialize.get <*> Serialize.get <*> Serialize.get
      go 3 = Sugar'Map <$> Serialize.get <*> Serialize.get
      go _ = fail "No matching Sugar value"
      
      getSerializedText :: Serialize.Get Text
      getSerializedText = do
        txt <- (decodeConvertText . UTF8) <$> (Serialize.get :: Serialize.Get BS.ByteString)
        maybe (fail "Cannot deserialize text as UTF8") pure txt
  
  put (Sugar'Unit note) = do
    Serialize.put (0 :: Word8)
    Serialize.put note
  put (Sugar'Text txt note) = do
    Serialize.put (1 :: Word8)
    Serialize.put (unUTF8 $ fromText txt :: BS.ByteString)
    Serialize.put note
  put (Sugar'List xs w note) = do
    Serialize.put (2 :: Word8)
    Serialize.put xs
    Serialize.put w
    Serialize.put note
  put (Sugar'Map m note) = do
    Serialize.put (3 :: Word8)
    Serialize.put m
    Serialize.put note  

instance Serialize.Serialize Wrap where

instance IsString Sugar where
  fromString str = Sugar'Text (toText str) Nothing

--

readSugarMay :: Read a => Sugar -> Maybe a
readSugarMay (Sugar'Text t _) = readMay $ T.unpack t
readSugarMay _ = Nothing

sugarMapAsIxMap :: [(Sugar,Sugar)] -> Map (Int, Sugar) Sugar
sugarMapAsIxMap = Map.fromList . zipWith (\i (k,v) -> ((i,k),v)) [0..]

class FromSugar a where
  parseSugar :: Sugar -> Maybe a

instance FromSugar a => FromSugar [a] where
  parseSugar (Sugar'List xs _ _) = mapM parseSugar xs
  parseSugar _ = Nothing

instance FromSugar a => FromSugar (Maybe a) where
  parseSugar (Sugar'Unit _) = Just Nothing
  parseSugar s = (return . Just) =<< parseSugar s

instance (FromSugar a, Ord a, FromSugar b) => FromSugar (Map a b) where
  parseSugar (Sugar'Map m _) = Just $ Map.fromList $
    mapMaybe
      (\(s,v) -> (,) <$> parseSugar s <*> parseSugar v)
      m
  parseSugar _ = Nothing

instance FromSugar Text where
  parseSugar (Sugar'Text t _) = Just t
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

--

sugarTextMay :: Sugar -> Maybe Text
sugarTextMay (Sugar'Text t _) = Just t
sugarTextMay _ = Nothing

class ToSugar a where
  toSugar :: a -> Sugar

instance ToSugar () where
  toSugar () = Sugar'Unit Nothing

instance ToSugar Text where
  toSugar t = Sugar'Text t Nothing

-- TODO: Will conflict with a String instance (aka [Char])
instance ToSugar a => ToSugar [a] where
  toSugar xs = Sugar'List (map toSugar xs) Wrap'Square Nothing

instance ToSugar a => ToSugar (Maybe a) where
  toSugar Nothing = Sugar'Unit Nothing
  toSugar (Just a) = toSugar a

instance (ToSugar a, ToSugar b) => ToSugar (Map a b) where
  toSugar m = Sugar'Map (map (\(k,v) -> (toSugar k, toSugar v)) $ Map.toList m) Nothing
  
instance (ToSugar a, ToSugar b) => ToSugar (a,b) where
  toSugar (a,b) = Sugar'List [toSugar a, toSugar b] Wrap'Paren Nothing

instance (ToSugar a, ToSugar b, ToSugar c) => ToSugar (a,b,c) where
  toSugar (a,b,c) = Sugar'List [toSugar a, toSugar b, toSugar c] Wrap'Paren Nothing

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

sugarShow :: Show a => a -> Sugar
sugarShow s = Sugar'Text (T.pack $ show s) Nothing

---

data PrettyPrintConfig = PrettyPrintConfig
  { ppcTabbedSpaces :: Int
  } deriving (Show, Eq)

data PrettyPrintState = PrettyPrintState
  { ppsNesting :: Int
  } deriving (Show, Eq)

prettyPrintSugarIO :: Sugar -> IO ()
prettyPrintSugarIO = TIO.putStr . prettyPrintSugar

prettyPrintSugar :: Sugar -> Text
prettyPrintSugar = prettyPrintSugar' (PrettyPrintConfig 2)

prettyPrintSugar' :: PrettyPrintConfig -> Sugar -> Text
prettyPrintSugar' ppc = prettyPrintStep ppc (PrettyPrintState 0)

prettyPrintNesting :: PrettyPrintConfig -> PrettyPrintState -> Text
prettyPrintNesting ppc pps = T.replicate (ppcTabbedSpaces ppc * ppsNesting pps) " "

ppsIncrNesting :: PrettyPrintState -> PrettyPrintState
ppsIncrNesting pps = pps { ppsNesting = ppsNesting pps + 1 }

ppsDecrNesting :: PrettyPrintState -> PrettyPrintState
ppsDecrNesting pps = pps { ppsNesting = if n >= 1 then n else 0 }
  where
    n = ppsNesting pps - 1

ppNewLine :: PrettyPrintConfig -> PrettyPrintState -> Text
ppNewLine ppc pps = "\n" <> prettyPrintNesting ppc pps 

prettyPrintStep :: PrettyPrintConfig -> PrettyPrintState -> Sugar -> Text
prettyPrintStep _ _ (Sugar'Unit note) = "()" <> minifyPrintNote note
prettyPrintStep _ _ (Sugar'Text txt note) = sanitizeText txt <> minifyPrintNote note
prettyPrintStep ppc pps (Sugar'List xs w note) =
    open
    <> T.concat (map (\x -> T.concat [ppNewLine ppc pps, prettyPrintStep ppc (ppsIncrNesting pps) x]) xs)
    <> ppNewLine ppc (ppsDecrNesting pps)
    <> close
    <> minifyPrintNote note
  where
    open, close :: Text
    (open,close) = case w of Wrap'Square -> ("[","]"); Wrap'Paren -> ("(",")")
prettyPrintStep ppc pps (Sugar'Map m note) = if ppsNesting pps == 0 && isNothing note then topLevel else nested
    where
      topLevel =
        T.concat (map (\(k,v) -> T.concat [prettyPrintStep ppc nextPps k, " ", prettyPrintStep ppc nextPps v, "\n"]) m)
      nested =
        "{"
        <> T.concat (map (\(k,v) -> T.concat [ppNewLine ppc pps, prettyPrintStep ppc nextPps k, " ", prettyPrintStep ppc nextPps v]) m)
        <> ppNewLine ppc (ppsDecrNesting pps)
        <> "}"
        <> minifyPrintNote note
      nextPps = ppsIncrNesting pps

minifyPrint :: Sugar -> Text
minifyPrint (Sugar'Unit note) = "()" <> minifyPrintNote note
minifyPrint (Sugar'Text txt note) = sanitizeText txt <> minifyPrintNote note
minifyPrint (Sugar'List xs w note) = open <> T.intercalate " " (map minifyPrint xs) <> close <> minifyPrintNote note
  where
    open, close :: Text
    (open,close) = case w of Wrap'Square -> ("[","]"); Wrap'Paren -> ("(",")")
minifyPrint (Sugar'Map m note) = "{" <> T.intercalate " " (map minifyPrint xs) <> "}" <> minifyPrintNote note
  where
    xs :: [Sugar]
    xs = (\(k,v) -> [k,v]) =<< m

minifyPrintNote :: Note -> Text
minifyPrintNote Nothing = ""
minifyPrintNote (Just s) = "<" <> minifyPrint s <> ">"

sanitizeText :: Text -> Text
sanitizeText t
  | T.length t == 0 = "\"\""
  | T.find (\c -> isSeparator c || elem c reservedChars) t /= Nothing = "\"" <> replaceDoubleQuotes t <> "\""
  | otherwise = t
  where
    replaceDoubleQuotes :: Text -> Text
    replaceDoubleQuotes = T.replace "\"" "\\\""
    
reservedChars :: [Char]
reservedChars = ['\"','[',']','<','>','(',')','{','}',';']

---
---

readSugarFromFile :: FilePath -> IO (Maybe Sugar)
readSugarFromFile path = do
  content <- TIO.readFile path
  return $ parseSugarFromText content

parseSugarFromText :: Text -> Maybe Sugar
parseSugarFromText t = case P.runParser sugarP "" t of
  Left _ -> Nothing
  Right s -> Just s
  
readSugarListFromFile :: FilePath -> IO (Maybe Sugar)
readSugarListFromFile path = do
  content <- TIO.readFile path
  return $ parseSugarListFromText content

parseSugarListFromText :: Text -> Maybe Sugar
parseSugarListFromText t = case P.runParser sugarNoBracketsListP "" t of 
  Left _ -> Nothing
  Right s -> Just s

---
---

type Parser = P.Parsec Void Text

sugarP :: Parser Sugar
sugarP = P.choice [P.try noCurlysMapP, sugarP']

sugarNoBracketsListP :: Parser Sugar
sugarNoBracketsListP = P.choice [P.try noBracketsListP, sugarP']

sugarP' :: Parser Sugar
sugarP' = do
  c <- P.lookAhead P.anySingle
  case c of
    '\"' -> quotedTextP
    '(' -> P.choice [unitP, parenListP]
    ')' -> fail "Not valid Sugar"
    '[' -> squareListP
    ']' -> fail "Not valid Sugar"
    '{' -> mapP
    '}' -> fail "Not valid Sugar"
    '<' -> fail "Not valid Sugar"
    '>' -> fail "Not valid Sugar"
    _ -> unQuotedTextP

unitP :: Parser Sugar
unitP = P.string "()" *> sc *> (Sugar'Unit <$> noteP)

parenListP, squareListP :: Parser Sugar
parenListP = (\xs -> Sugar'List xs Wrap'Paren) <$> parensP (P.many sugarP') <*> noteP
squareListP = (\xs -> Sugar'List xs Wrap'Square) <$> (squareBracketsP $ sc *> P.many elementP <* sc) <*> noteP
  where
    elementP :: Parser Sugar
    elementP = sc *> sugarP' <* sc

noBracketsListP :: Parser Sugar
noBracketsListP = (\xs -> Sugar'List xs Wrap'Square) <$> (sc *> P.many elementP <* sc) <*> pure Nothing
  where
    elementP :: Parser Sugar
    elementP = sc *> sugarP' <* sc

mapP, noCurlysMapP :: Parser Sugar
mapP = Sugar'Map <$> (curlyBracesP $ sc *> P.many mapPairP <* sc) <*> noteP
noCurlysMapP = Sugar'Map <$> (sc *> P.many mapPairP <* sc) <*> pure Nothing

-- TODO: Instead of `P.space1`, use the same characters for `isSeparator`
mapPairP :: Parser (Sugar, Sugar)
mapPairP = (,) <$> sugarP' <*> (sc *> sugarP') <* sc

noteP :: Parser Note
noteP = P.optional $ angleBracketsP sugarP'

parensP, angleBracketsP, squareBracketsP, curlyBracesP :: Parser a -> Parser a
parensP = P.between (symbol "(") (symbol ")")
angleBracketsP = P.between (symbol "<") (symbol ">")
squareBracketsP = P.between (symbol "[") (symbol "]")
curlyBracesP = P.between (symbol "{") (symbol "}")

symbol :: Text -> Parser Text
symbol = L.symbol sc

quotedTextP, unQuotedTextP :: Parser Sugar
quotedTextP = Sugar'Text <$> doubleQuotedTextP_ <*> (sc *> noteP)
unQuotedTextP = Sugar'Text <$> notQuotedTextP_ <*> noteP

doubleQuotedTextP_, notQuotedTextP_ :: Parser Text
doubleQuotedTextP_ = T.pack <$> quotedP
  where
    quotedP :: Parser String
    quotedP = P.between (P.char '\"') (P.char '\"') (many (P.try escaped <|> normalChar))
       where
         escaped = '\"' <$ P.string "\\\""
         normalChar = P.satisfy (/='\"')
notQuotedTextP_ = P.takeWhileP (Just "Text char") (\c -> not $ isSeparator c || c == '\n' || elem c reservedChars)

sc :: Parser ()
sc = L.space
  ws
  (L.skipLineComment ";") -- TODO replace with ';' once issue 88 is fixed
  (L.skipBlockComment "#|" "|#")
  
ws :: Parser ()
ws = (P.newline <|> P.separatorChar) *> pure ()
