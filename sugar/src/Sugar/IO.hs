{-# LANGUAGE TupleSections, DeriveGeneric, OverloadedStrings, CPP #-}
module Sugar.IO
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
  , sugarLexerState
  ) where

import Data.Text (Text)
import Data.Maybe (isNothing)
import Data.Char -- (isSeparator)

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Text.Megaparsec as P

import Sugar.Types
import Sugar.Parse
import Sugar.Lexer

--

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
