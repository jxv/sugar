{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Sugar.Lexer
  ( LexerState(..)
  , LexemeStep
  , SourceLocation(..)
  , Lexeme(..)
  , sugarLexerState
  ) where

import Data.Char
import Safe.Exact (splitAtExactMay)

import Sugar.Types (reservedChars)

data LexerState = LexerState
  { psSteps :: [LexemeStep]
  , psLocation :: SourceLocation
  } deriving (Show, Eq)

type LexemeStep = (SourceLocation, Lexeme)

data SourceLocation = SourceLocation
  { slLine :: Int
  , slColumn :: Int
  } deriving (Show, Eq)

data Lexeme
  = Start
  | OpenCurl
  | CloseCurl
  | OpenParen
  | CloseParen
  | OpenSquare
  | CloseSquare
  | OpenAngle
  | CloseAngle
  | Comma
  | Colon
  | StringStart
  | String String
  | QuoteStart
  | QuotedString String
  | QuoteEnd
  | SingleLineComment
  | MultiLineCommentStart
  | MultiLineCommentEnd
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
    initLexerState = LexerState [] (SourceLocation 1 1)

incrColLoc :: SourceLocation -> SourceLocation
incrColLoc sl = sl { slColumn = slColumn sl + 1 }

stepColLoc :: Int -> SourceLocation -> SourceLocation
stepColLoc n sl = sl { slColumn = slColumn sl + n }

nextLineLoc :: SourceLocation -> SourceLocation
nextLineLoc sl = sl { slLine = slLine sl + 1, slColumn = 1 }

incrColState :: LexerState -> LexerState
incrColState ps = ps { psLocation = incrColLoc $ psLocation ps }

stepColState :: Int -> LexerState -> LexerState
stepColState n ps = ps { psLocation = stepColLoc n $ psLocation ps }

nextLineState :: LexerState -> LexerState
nextLineState ps = ps { psLocation = nextLineLoc $ psLocation ps }

stepLoc :: String -> SourceLocation -> SourceLocation
stepLoc [] loc = loc
stepLoc (x:xs) loc
  | x == '\n' = stepLoc xs (nextLineLoc loc)
  | otherwise = stepLoc xs (incrColLoc loc) -- assuming non-zero width characters

stepLocState :: String -> LexerState -> LexerState
stepLocState s ps = ps { psLocation = stepLoc s $ psLocation ps }

lastLexeme :: LexerState -> Maybe Lexeme
lastLexeme ps = case psSteps ps of
  ((_,t):_) -> Just t
  [] -> Nothing

stepReadSugarString :: String -> LexerState -> (String, LexerState)
stepReadSugarString s ps = case lastLexeme ps of
  Nothing -> stepReadSugarString' s ps Start -- Benign hack to start parsing
  Just t -> stepReadSugarString' s ps t

stepReadSugarString' :: String -> LexerState -> Lexeme -> (String, LexerState)
stepReadSugarString' [] ps _ = ([], ps)
stepReadSugarString' s ps t = case t of
  StringStart -> stepReadString s ps
  QuoteStart -> stepQuotedStart s ps
  QuotedString _ -> stepQuoteString s ps
  SingleLineComment -> stepSingleLineComment s ps
  MultiLineCommentStart -> stepMultiLineComment s ps
  _ -> normalStepReadSugarString s ps

normalStepReadSugarString :: String -> LexerState -> (String, LexerState)
normalStepReadSugarString [] ps = ([], ps)
normalStepReadSugarString s@(c:cs) ps
  | c == '\n' = (cs, nextLineState ps)
  | isSpace c = (cs, incrColState ps)
  | otherwise = case c of
    '{' -> (cs, step OpenCurl)
    '}' -> (cs, step CloseCurl)
    '(' -> (cs, step OpenParen)
    ')' -> (cs, step CloseParen)
    '[' -> (cs, step OpenSquare)
    ']' -> (cs, step CloseSquare)
    '<' -> (cs, step OpenAngle)
    '>' -> (cs, step CloseAngle)
    ',' -> (cs, step Comma)
    ':' -> (cs, step Colon)
    '"' -> (cs, step QuoteStart)
    ';' -> (cs, step SingleLineComment)
    _ -> case splitAtExactMay 2 s of
      Just ("#|", s') ->
        (s', stepColState 2 $ ps{psSteps = (psLocation ps, MultiLineCommentStart) : psSteps ps})
      _ -> (s, ps { psSteps = (psLocation ps, StringStart) : psSteps ps })
    where
      step t = (incrColState ps) { psSteps = (psLocation ps, t) : psSteps ps }

prependStep :: LexemeStep -> LexerState -> LexerState
prependStep s ps = ps { psSteps = s : psSteps ps }

stepReadString :: String -> LexerState -> (String, LexerState)
stepReadString s ps = (s', ps')
  where
    ps' = stepColState (length str) $ prependStep step ps
    step = (psLocation ps, String str)
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
    ps' = stepColState (length str) $ prependStep step ps
    step = (psLocation ps, String str)
    (str, s') = span (/= '\n') s

stepMultiLineComment :: String -> LexerState -> (String, LexerState)
stepMultiLineComment s ps =  case span2ExactSkip (\c c' -> c == '|' && c' == '#') s of
  Nothing -> ("", stepLocState s ps) -- failed to consume end of comment marker
  Just (str, s') -> let
    step = (psLocation ps, MultiLineCommentEnd)
    ps' = stepLocState (str ++ "|#") $ prependStep step ps
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
    ps' = stepColState (length str) $ prependStep step ps
    step = (loc, QuotedString str)
    (loc, str, s') = spanWithEscape (psLocation ps) s

spanWithEscape :: SourceLocation -> String -> (SourceLocation, String, String)
spanWithEscape loc [] = (loc, [],[])
spanWithEscape loc (x:[]) = if x == '"' then (incrColLoc loc, [],[x]) else (loc, [x],[])
spanWithEscape loc xs@(x:y:z) = case x of
  '"' -> (incrColLoc loc, [],xs)
  '\\' -> case spanWithEscape loc z of
    (loc', ys,zs) -> (incrColLoc loc', x:y:ys, zs)
  '\n' -> let (loc', ys,zs) = spanWithEscape loc (y:z) in (nextLineLoc loc', x:ys, zs)
  _ -> let (loc', ys,zs) = spanWithEscape loc (y:z) in (incrColLoc loc', x:ys, zs)

stepQuoteString :: String -> LexerState -> (String, LexerState)
stepQuoteString ('"':xs) ps = (xs, incrColState $ prependStep (psLocation ps, QuoteEnd) ps)
stepQuoteString xs ps = normalStepReadSugarString xs ps -- Something went wrong, but keep parsing.
