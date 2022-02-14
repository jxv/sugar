{-# LANGUAGE TupleSections, DeriveGeneric, OverloadedStrings, CPP #-}
module Data.Sugar where

import Control.Applicative (Alternative(..))
import Data.Void (Void)
import Data.Text (Text)
import Data.Map (Map)
import Data.Maybe (isNothing)
import Data.Text.Conversions (ToText(..), fromText, unUTF8, decodeConvertText, UTF8(..))
import Data.String (IsString(..))
import Data.Word (Word8,Word16,Word32,Word64)
import Data.Int (Int8,Int16,Int32,Int64)
import Data.Scientific (floatingOrInteger)
import Data.Char (isSeparator)
import GHC.Generics (Generic)
import TextShow (TextShow, showt)

import qualified Data.Set as Set
import qualified Data.Map as Map

#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as KeyMap
#else
-- *import qualified Data.Aeson.Key as AesonKey
import qualified Data.HashMap.Strict as KeyMap
#endif

import qualified Data.Serialize as Serialize
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Aeson as Json
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L

---

class Similar a where
  (~~) :: a -> a -> Bool
  (~~) x y = not (x /~ y)
  (/~) :: a -> a -> Bool
  (/~) x y = not (x ~~ y)

instance Ord a => Similar [a] where
  (~~) a b = Set.fromList a == Set.fromList b

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
  
class FromSugar a where
  parseSugar :: Sugar -> Maybe a
  
data Color
  = Color'R
  | Color'G
  | Color'B
  deriving (Show, Eq)

instance FromSugar Color where
  parseSugar (Sugar'Text a _) = case a of
    "R" -> Just Color'R
    "G" -> Just Color'G
    "B" -> Just Color'B
    _ -> Nothing
  parseSugar _ = Nothing
  
instance FromSugar a => FromSugar [a] where
  parseSugar (Sugar'List xs _ _) = mapM parseSugar xs
  parseSugar _ = Nothing
    

sugarCubeMay :: Sugar -> Maybe SugarCube
sugarCubeMay (Sugar'Unit _) = Just SugarCube'Unit
sugarCubeMay (Sugar'Text t _) = Just $ SugarCube'Text t
sugarCubeMay (Sugar'List xs _ _) = do
  xs' <- mapM sugarCubeMay xs
  return $ SugarCube'List xs'
sugarCubeMay (Sugar'Map xs _) = do
  xs' <- mapM (\(k,v) -> (,) <$> sugarTextMay k <*> sugarCubeMay v) xs
  return $ SugarCube'Map (Map.fromList xs')

sugarTextMay :: Sugar -> Maybe Text
sugarTextMay (Sugar'Text t _) = Just t
sugarTextMay _ = Nothing

type Note = Maybe Sugar

data Wrap
  = Wrap'Square
  | Wrap'Paren
  deriving (Eq, Show, Generic)

instance Serialize.Serialize Wrap where

data Sugar
  = Sugar'Unit Note
  | Sugar'Text Text Note
  | Sugar'List [Sugar] Wrap Note
  | Sugar'Map [(Sugar,Sugar)] Note
  deriving (Eq, Show, Generic)

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

instance IsString Sugar where
  fromString str = Sugar'Text (toText str) Nothing

class ToSugar a where
  toSugar :: a -> Sugar

instance ToSugar SugarCube where
  toSugar SugarCube'Unit = Sugar'Unit Nothing
  toSugar (SugarCube'Text t) = Sugar'Text t Nothing
  toSugar (SugarCube'List xs) = Sugar'List (map (toSugarWithWrap Wrap'Paren) xs) Wrap'Square Nothing
    where
      -- Alternate nesting between Wrap types
      toSugarWithWrap w c = case c of
        SugarCube'List ys -> Sugar'List (map (toSugarWithWrap (case w of Wrap'Square -> Wrap'Paren; Wrap'Paren -> Wrap'Square)) ys) w Nothing
        _ -> toSugar c
  toSugar (SugarCube'Map m) = Sugar'Map (map (\(k,v) -> (toSugar k, toSugar v)) $ Map.toList m) Nothing

instance ToSugar () where
  toSugar () = Sugar'Unit Nothing

instance ToSugar Text where
  toSugar t = Sugar'Text t Nothing

-- TODO: Review this if it causes problems in the REPL
instance ToSugar a => ToSugar [a] where
  toSugar xs = Sugar'List (map toSugar xs) Wrap'Square Nothing

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

sugarShow :: TextShow a => a -> Sugar
sugarShow s = Sugar'Text (showt s) Nothing

--

mySugar :: Sugar
mySugar = Sugar'Map [(Sugar'Text "key" Nothing, Sugar'Text "value" Nothing)] Nothing

mySugar' :: Sugar
mySugar' = Sugar'Map [
    (Sugar'Text "key" (Just "A-che-ora"), Sugar'Text "value" (Just "A che ora")),
    (Sugar'Text "anotherKey" Nothing, Sugar'Map
      [
        (Sugar'Text "a" Nothing, Sugar'Text "12345\"6789" Nothing),
        (Sugar'Text "c" Nothing, Sugar'Text "12345    6789" Nothing),
        (Sugar'Text "e" Nothing, Sugar'Text "123'456789" Nothing),
        (Sugar'Text "d" Nothing, Sugar'List ["", "", Sugar'List [Sugar'List [Sugar'List ["","",Sugar'Map [("",Sugar'Map [] Nothing),("",Sugar'Map [] Nothing)] Nothing] Wrap'Paren Nothing] Wrap'Square Nothing]Wrap'Paren Nothing] Wrap'Square Nothing)
      ]
      (Just "Hello"))
  ] Nothing

---

instance ToSugarCube Json.Value where
  toSugarCube Json.Null = SugarCube'Unit
  toSugarCube (Json.Bool b) = SugarCube'Text (showt b)
  toSugarCube (Json.String t) = SugarCube'Text t
  toSugarCube (Json.Number n) = SugarCube'Text (showNumber n)
    where
      showNumber s = either showt showt $ (floatingOrInteger s :: Either Double Integer)
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
    Just sugar ->  TIO.writeFile des $ prettyPrint sugar


data PrettyPrintConfig = PrettyPrintConfig
  { ppcTabbedSpaces :: Int
  } deriving (Show, Eq)
  
data PrettyPrintState = PrettyPrintState
  { ppsNesting :: Int
  } deriving (Show, Eq)

prettyPrintIO :: Sugar -> IO ()
prettyPrintIO = TIO.putStr . prettyPrint

prettyPrint :: Sugar -> Text
prettyPrint = prettyPrint' (PrettyPrintConfig 2)

prettyPrint' :: PrettyPrintConfig -> Sugar -> Text
prettyPrint' ppc = prettyPrintStep ppc (PrettyPrintState 0)

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
  | T.length t == 0 = "''"
  | T.find (\c -> isSeparator c || elem c reservedChars) t /= Nothing = "'" <> replaceSingleQuotes t <> "'"
  | otherwise = t
  where
    replaceSingleQuotes :: Text -> Text
    replaceSingleQuotes = T.replace "'" "''"
    
reservedChars :: [Char]
reservedChars = ['\'','[',']','<','>','(',')','{','}',';']

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
    '\'' -> quotedTextP
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

parenListP :: Parser Sugar
parenListP = (\xs -> Sugar'List xs Wrap'Paren) <$> parensP (P.many sugarP') <*> noteP

squareListP :: Parser Sugar
squareListP = (\xs -> Sugar'List xs Wrap'Square) <$> (squareBracketsP $ sc *> P.many elementP <* sc) <*> noteP
  where
    elementP :: Parser Sugar
    elementP = sc *> sugarP' <* sc

noBracketsListP :: Parser Sugar
noBracketsListP = (\xs -> Sugar'List xs Wrap'Square) <$> (sc *> P.many elementP <* sc) <*> pure Nothing
  where
    elementP :: Parser Sugar
    elementP = sc *> sugarP' <* sc

mapP :: Parser Sugar
mapP = Sugar'Map <$> (curlyBracesP $ sc *> P.many mapPairP <* sc) <*> noteP

noCurlysMapP :: Parser Sugar
noCurlysMapP = Sugar'Map <$> (sc *> P.many mapPairP <* sc) <*> pure Nothing

-- TODO: Instead of `P.space1`, use the same characters for `isSeparator`
mapPairP :: Parser (Sugar, Sugar)
mapPairP = (,) <$> sugarP' <*> (sc *> sugarP') <* sc

noteP :: Parser Note
noteP = P.optional $ angleBracketsP sugarP'

parensP :: Parser a -> Parser a
parensP = P.between (symbol "(") (symbol ")")

angleBracketsP  :: Parser a -> Parser a
angleBracketsP = P.between (symbol "<") (symbol ">")

squareBracketsP  :: Parser a -> Parser a
squareBracketsP = P.between (symbol "[") (symbol "]")

curlyBracesP  :: Parser a -> Parser a
curlyBracesP = P.between (symbol "{") (symbol "}")

symbol :: Text -> Parser Text
symbol = L.symbol sc

quotedTextP :: Parser Sugar
quotedTextP = Sugar'Text <$> singleQuotedTextP_ <*> (sc *> noteP)

unQuotedTextP :: Parser Sugar
unQuotedTextP = Sugar'Text <$> notQuotedTextP_ <*> noteP

singleQuotedTextP_ :: Parser Text
singleQuotedTextP_ = T.pack <$> quotedP
  where
    quotedP :: Parser String
    quotedP = P.between (P.char '\'') (P.char '\'') (many (P.try escaped <|> normalChar))
       where
         escaped = '\'' <$ P.string "''"
         normalChar = P.satisfy (/='\'')

notQuotedTextP_ :: Parser Text
notQuotedTextP_ = P.takeWhileP (Just "Text char") (\c -> not $ isSeparator c || c == '\n' || elem c reservedChars)

sc :: Parser ()
sc = L.space
  ws
  (L.skipLineComment ";") -- TODO replace with ';' once issue 88 is fixed
  (L.skipBlockComment "#|" "|#")
  
ws :: Parser ()
ws = (P.newline <|> P.separatorChar) *> pure ()

---

keyVal :: IO ()
keyVal = readSugarFromFile "keyValue.sg" >>= \(Just sg) -> prettyPrintIO sg

large :: IO ()
large = readSugarFromFile "large.sg" >>= \(Just sg) -> prettyPrintIO sg

mysugar :: IO ()
mysugar = readSugarFromFile "mysugar.sg" >>= \(Just sg) -> prettyPrintIO sg

noline :: IO ()
noline = readSugarFromFile "noline.sg" >>= \(Just sg) -> prettyPrintIO sg
