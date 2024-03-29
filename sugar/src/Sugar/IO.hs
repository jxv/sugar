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

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Sugar.Parser (flatten, runParser, sugarParseList, sugarParseTopLevel)
import Sugar.Types
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
prettyPrintStep _ _ (Unit note) = "()" <> minifyPrintNote note
prettyPrintStep _ _ (Text txt NoQuote note) =  txt <> minifyPrintNote note
prettyPrintStep _ _ (Text txt HasQuote note) = "\"" <> txt <> "\"" <> minifyPrintNote note
prettyPrintStep ppc pps (List xs w note) =
    open
    <> T.concat (map (\x -> T.concat [ppNewLine ppc pps, prettyPrintStep ppc (ppsIncrNesting pps) x]) xs)
    <> ppNewLine ppc (ppsDecrNesting pps)
    <> close
    <> minifyPrintNote note
  where
    open, close :: Text
    (open,close) = case w of Square -> ("[","]"); Paren -> ("(",")")
prettyPrintStep ppc pps (Map m note) = if ppsNesting pps == 0 && isNothing note then topLevel else nested
    where
      topLevel =
        T.concat (map (\(k,v) -> T.concat [prettyPrintStep ppc nextPps k, ": ", prettyPrintStep ppc nextPps v, "\n"]) m)
      nested =
        "{"
        <> T.concat (map (\(k,v) -> T.concat [ppNewLine ppc pps, prettyPrintStep ppc nextPps k, ": ", prettyPrintStep ppc nextPps v]) m)
        <> ppNewLine ppc (ppsDecrNesting pps)
        <> "}"
        <> minifyPrintNote note
      nextPps = ppsIncrNesting pps

minifyPrint :: Sugar -> Text
minifyPrint (Unit note) = "()" <> minifyPrintNote note
minifyPrint (Text txt NoQuote note) = txt <> minifyPrintNote note
minifyPrint (Text txt HasQuote note) = "\"" <> txt <> "\"" <> minifyPrintNote note
minifyPrint (List xs w note) = open <> T.intercalate ":" (map minifyPrint xs) <> close <> minifyPrintNote note
  where
    open, close :: Text
    (open,close) = case w of Square -> ("[","]"); Paren -> ("(",")")
minifyPrint (Map m note) = "{" <> T.intercalate "," (map minifyPrint xs) <> "}" <> minifyPrintNote note
  where
    xs :: [Sugar]
    xs = (\(k,v) -> [k,v]) =<< m

minifyPrintNote :: Note -> Text
minifyPrintNote Nothing = ""
minifyPrintNote (Just xs) = "<" <> T.intercalate " " (map minifyPrint xs) <> ">"

readSugarFromFile :: FilePath -> IO (Maybe Sugar)
readSugarFromFile path = do
  content <- TIO.readFile path
  return $ parseSugarFromText content

parseSugarFromText :: Text -> Maybe Sugar
parseSugarFromText t = case runParser sugarParseTopLevel (psSteps $ sugarLexerState (T.unpack t)) of
  (_, Left _) -> Nothing
  (_, Right s) -> Just $ flatten s

readSugarListFromFile :: FilePath -> IO (Maybe Sugar)
readSugarListFromFile path = do
  content <- TIO.readFile path
  return $ parseSugarListFromText content

parseSugarListFromText :: Text -> Maybe Sugar
parseSugarListFromText t = case runParser sugarParseList (psSteps $ sugarLexerState (T.unpack t)) of
  (_, Left _) -> Nothing
  (_, Right s) -> Just $ flatten s
