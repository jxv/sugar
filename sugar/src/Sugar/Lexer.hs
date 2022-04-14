{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Sugar.Lexer where

import Control.Monad
import Control.Applicative
import Data.Void (Void)
import Data.Text (Text)
import Data.Char
import Safe.Exact (splitAtExactMay)

import Sugar.Types

data LexerState = LexerState
  { psSteps :: [TokenStep]
  , psLocation :: SourceLocation
  } deriving (Show, Eq)

type TokenStep = (SourceLocation, Token)

data SourceLocation = SourceLocation
  { slLine :: Int
  , slColumn :: Int
  } deriving (Show, Eq)

data Token
  = Token'Start
  | Token'OpenCurl
  | Token'CloseCurl
  | Token'OpenParen
  | Token'CloseParen
  | Token'OpenSquare
  | Token'CloseSquare
  | Token'OpenAngle
  | Token'CloseAngle
  | Token'StringStart
  | Token'String String
  | Token'QuoteStart
  | Token'QuotedString String
  | Token'QuoteEnd
  | Token'SingleLineComment
  | Token'MultiLineCommentStart
  | Token'MultiLineCommentEnd
  deriving (Show, Eq)

sugarLexerState :: String -> LexerState
sugarLexerState s =
  let ls = go (stepReadSugarString s initLexerState)
  in ls { psSteps = reverse (psSteps ls) }
  where
    go (s',ps) = case s' of
      [] -> ps
      _ -> go (stepReadSugarString s' ps)
    initLexerState :: LexerState
    initLexerState = LexerState [] (SourceLocation 0 0)

incrCol :: SourceLocation -> SourceLocation
incrCol sl = sl { slColumn = slColumn sl + 1 }

stepCol :: Int -> SourceLocation -> SourceLocation
stepCol n sl = sl { slColumn = slColumn sl + n }

nextLine :: SourceLocation -> SourceLocation
nextLine sl = sl { slLine = slLine sl + 1, slColumn = 0 }

incrCol' :: LexerState -> LexerState
incrCol' ps = ps { psLocation = incrCol $ psLocation ps }

stepCol' :: Int -> LexerState -> LexerState
stepCol' n ps = ps { psLocation = stepCol n $ psLocation ps }

nextLine' :: LexerState -> LexerState
nextLine' ps = ps { psLocation = nextLine $ psLocation ps }

stepLoc :: String -> SourceLocation -> SourceLocation
stepLoc [] loc = loc
stepLoc (x:xs) loc
  | x == '\n' = stepLoc xs (nextLine loc)
  | otherwise = stepLoc xs (incrCol loc) -- assuming non-zero width characters

stepLoc' :: String -> LexerState -> LexerState
stepLoc' s ps = ps { psLocation = stepLoc s $ psLocation ps }

lastToken :: LexerState -> Maybe Token
lastToken ps = case psSteps ps of
  ((_,t):_) -> Just t
  [] -> Nothing

stepReadSugarString :: String -> LexerState -> (String, LexerState)
stepReadSugarString s ps = case lastToken ps of
  Nothing -> stepReadSugarString' s ps Token'Start -- Benign hack to start parsing
  Just t -> stepReadSugarString' s ps t

stepReadSugarString' :: String -> LexerState -> Token -> (String, LexerState)
stepReadSugarString' [] ps _ = ([], ps)
stepReadSugarString' s ps t = case t of
  Token'StringStart -> stepReadString s ps
  Token'QuoteStart -> stepQuotedStart s ps
  Token'QuotedString _ -> stepQuoteString s ps
  Token'SingleLineComment -> stepSingleLineComment s ps
  Token'MultiLineCommentStart -> stepMultiLineComment s ps
  _ -> normalStepReadSugarString s ps

normalStepReadSugarString :: String -> LexerState -> (String, LexerState)
normalStepReadSugarString [] ps = ([], ps)
normalStepReadSugarString s@(c:cs) ps
  | c == '\n' = (cs, nextLine' ps)
  | isSpace c = (cs, incrCol' ps)
  | otherwise = case c of
    '{' -> (cs, step Token'OpenCurl)
    '}' -> (cs, step Token'CloseCurl)
    '(' -> (cs, step Token'OpenParen)
    ')' -> (cs, step Token'CloseParen)
    '[' -> (cs, step Token'OpenSquare)
    ']' -> (cs, step Token'CloseSquare)
    '<' -> (cs, step Token'OpenAngle)
    '>' -> (cs, step Token'CloseAngle)
    '"' -> (cs, step Token'QuoteStart)
    ';' -> (cs, step Token'SingleLineComment)
    _ -> case splitAtExactMay 2 s of
      Just ("#|", s') ->
        (s', stepCol' 2 $ ps{psSteps = (psLocation ps, Token'MultiLineCommentStart) : psSteps ps})
      _ -> (s, ps { psSteps = (psLocation ps, Token'StringStart) : psSteps ps })
    where
      step t = (incrCol' ps) { psSteps = (psLocation ps, t) : psSteps ps }

prependStep :: TokenStep -> LexerState -> LexerState
prependStep s ps = ps { psSteps = s : psSteps ps }

stepReadString :: String -> LexerState -> (String, LexerState)
stepReadString s ps = (s', ps')
  where
    ps' = stepCol' (length str) $ prependStep step ps
    step = (psLocation ps, Token'String str)
    (str, s') = span2
      (\c c' -> not $ isSpace c || isReservedChar c || (c == '#' && c' == Just '|'))
      s

isReservedChar :: Char -> Bool
isReservedChar = flip elem reservedChars

span2 :: (a -> Maybe a -> Bool) -> [a] -> ([a],[a])
span2 _ [] = ([],[])
span2 f (x:[]) = if f x Nothing then ([x],[]) else ([],[x])
span2 f xs@(x:y:z)
  | f x (Just y) = let (ys,zs) = span2 f (y:z) in (x:ys, zs)
  | otherwise = ([], xs)

stepSingleLineComment :: String -> LexerState -> (String, LexerState)
stepSingleLineComment s ps = (s', ps')
  where
    ps' = stepCol' (length str) $ prependStep step ps
    step = (psLocation ps, Token'String str)
    (str, s') = span (== '\n') s

stepMultiLineComment :: String -> LexerState -> (String, LexerState)
stepMultiLineComment s ps =  case span2ExactSkip (\c c' -> c == '|' && c' == '#') s of
  Nothing -> ("", stepLoc' s ps) -- failed to consume end of comment marker
  Just (str, s') -> let
    step = (psLocation ps, Token'MultiLineCommentEnd)
    ps' = stepLoc' (str ++ "|#") $ prependStep step ps
    in (s', ps')

span2ExactSkip :: (a -> a -> Bool) -> [a] -> Maybe ([a], [a])
span2ExactSkip _ [] = Nothing
span2ExactSkip _ (_:[]) = Nothing
span2ExactSkip f (x:y:z)
  | f x y = Just ([], z)
  | otherwise = fmap (\(a,b) -> (x:a, b)) (span2ExactSkip f (y:z))

stepQuotedStart :: String -> LexerState -> (String, LexerState)
stepQuotedStart s ps = (s', ps')
  where
    ps' = stepCol' (length str) $ prependStep step ps
    step = (loc, Token'QuotedString str)
    (loc, str, s') = spanWithEscape (psLocation ps) s

spanWithEscape :: SourceLocation -> String -> (SourceLocation, String, String)
spanWithEscape loc [] = (loc, [],[])
spanWithEscape loc (x:[]) = if x == '"' then (incrCol loc, [],[x]) else (loc, [x],[])
spanWithEscape loc xs@(x:y:z) = case x of
  '"' -> (incrCol loc, [],xs)
  '\\' -> case spanWithEscape loc z of
    (loc', ys,zs) -> (incrCol loc', x:y:ys, zs)
  '\n' -> let (loc', ys,zs) = spanWithEscape loc (y:z) in (nextLine loc', x:ys, zs)
  _ -> let (loc', ys,zs) = spanWithEscape loc (y:z) in (incrCol loc', x:ys, zs)

stepQuoteString :: String -> LexerState -> (String, LexerState)
stepQuoteString ('"':xs) ps = (xs, incrCol' $ prependStep (psLocation ps, Token'QuoteEnd) ps)
stepQuoteString xs ps = normalStepReadSugarString xs ps -- Something went wrong, but keep parsing.
