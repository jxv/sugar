{-# LANGUAGE TupleSections, DeriveGeneric, OverloadedStrings, CPP #-}
module Sugar.Parse
  ( sugarP
  , sugarNoBracketsListP
  , sugarLexerState
  ) where

import Control.Applicative (Alternative(..))
import Data.Void (Void)
import Data.Text (Text)
import Data.Char
import Safe.Exact (splitAtExactMay)

import qualified Data.Text as T
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L

import Sugar.Types


--

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

--
-- Lexer
sugarLexerState :: String -> LexerState
sugarLexerState s = go (stepReadSugarString s initLexerState)
  where
    go (s',ps) = case s' of
      [] -> ps
      _ -> go (stepReadSugarString s' ps)
    initLexerState :: LexerState
    initLexerState = LexerState [] (SourceLocation 0 0)

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
