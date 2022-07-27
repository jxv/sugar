{-# LANGUAGE TupleSections, DeriveGeneric, OverloadedStrings, CPP, NamedFieldPuns #-}
module Sugar.Parser where

import Control.Monad
import Control.Applicative

import qualified Data.Text as T

import Sugar.Types
import Sugar.Lexer

data Scan
  = Scan'Unit ScanNote
  | Scan'String String ScanNote
  | Scan'List [ScanStep] Wrap ScanNote
  | Scan'Map [(ScanStep,ScanStep)] ScanNote
  deriving (Show, Eq)

type ScanStep = (SourceLocation, Scan)
type ScanNote = Maybe [ScanStep]
type ParseError = (Maybe SourceLocation, String)

newtype Parser a = Parser
  { runParser :: [TokenStep] -> ([TokenStep], Either ParseError a) }

instance Functor Parser where
  fmap f (Parser g) = Parser $ \ts -> let (ts', x) = g ts in (ts', fmap f x)

instance Applicative Parser where
  pure x = Parser $ \ts -> (ts, Right x)
  p <*> q = Parser $ \ts -> let (ts',x) = runParser p ts in case x of
    Left a -> (ts', Left a)
    Right b -> runParser (fmap b q) ts'

instance Alternative Parser where
  empty = Parser $ \ts -> (ts, Left (Nothing, ""))
  p1 <|> p2 = Parser $ \ts -> case runParser p1 ts of
    (ts', Left err)
      | ts' == ts   -> runParser p2 ts
      | otherwise -> (ts', Left err)
    success -> success

instance Monad Parser where
  (Parser p) >>= f = Parser $ \ts -> let (ts',x) = p ts in case x of
    Left a -> (ts', Left a)
    Right b -> runParser (f b) ts'

--

flatten :: ScanStep -> Sugar
flatten (_, s) = case s of
  Scan'Unit note -> Sugar'Unit (fmap flatten <$> note)
  Scan'String str note -> Sugar'Text (T.pack str) (fmap flatten <$> note)
  Scan'List elems wrap note -> Sugar'List (flatten <$> elems) wrap (fmap flatten <$> note)
  Scan'Map elems note -> Sugar'Map ((\(x,y) -> (flatten x, flatten y)) <$> elems) (fmap flatten <$> note)

--

sugarParse :: Parser ScanStep
sugarParse = do
  (loc, tkn) <- peek
  case tkn of
    Token'Start -> sugarParse
    Token'OpenCurl -> sugarParseMap
    Token'OpenParen -> try sugarParseUnit <|> sugarParseParenList
    Token'OpenSquare -> sugarParseSquareList
    Token'QuoteStart -> sugarParseQuote
    Token'StringStart -> sugarParseText
    Token'SingleLineComment -> token Token'SingleLineComment *> ignoreUntilNewLine loc *> sugarParse
    _ -> sugarParseUnexpected (loc, tkn)

ignoreUntilNewLine :: SourceLocation -> Parser ()
ignoreUntilNewLine sl = do
  tkn' <- tryPeek
  case tkn' of
    Nothing -> return ()
    Just (loc,_) -> if slLine loc == slLine sl
      then nextToken *> ignoreUntilNewLine loc
      else return ()

sugarParseUnexpected :: TokenStep -> Parser ScanStep
sugarParseUnexpected (loc, tkn) = Parser $ \ts -> (ts, Left (Just loc, "Unexpected: " ++ show tkn))

sugarParseNote :: Parser (Maybe [ScanStep])
sugarParseNote = do
  tkn' <- tryPeek
  case tkn' of
    Nothing -> return Nothing
    Just (_,tkn) -> case tkn of
      Token'OpenAngle -> fmap pure $ between (token Token'OpenAngle) (token Token'CloseAngle) (many sugarParse)
      _ -> pure Nothing

sugarParseUnit :: Parser ScanStep
sugarParseUnit = do
  (sl,  _) <- token Token'OpenParen
  (sl', _) <- token Token'CloseParen
  if slColumn sl + 1 ==  slColumn sl' -- no space between parens
    then do
      note <- sugarParseNote
      let tkn = Scan'Unit note
      pure (sl, tkn)
    else
      empty

sugarParseTopLevel :: Parser ScanStep
sugarParseTopLevel = sugarParseTopLevelMap

sugarParseMap :: Parser ScanStep
sugarParseMap = do
  (sl, _) <- token Token'OpenCurl
  elems <- many ((,) <$> sugarParse <*> sugarParse)
  void $ token Token'CloseCurl
  note <- sugarParseNote
  let tkn = Scan'Map elems note
  pure (sl, tkn)

sugarParseTopLevelMap :: Parser ScanStep
sugarParseTopLevelMap = do
  elems <- many ((,) <$> sugarParse <*> sugarParse)
  let tkn = Scan'Map elems Nothing
  case elems of
    (((sl,_), _):_) -> return (sl, tkn)
    [] -> return (SourceLocation 0 0, tkn)

sugarParseList :: Parser ScanStep
sugarParseList = try sugarParseSquareList <|> sugarParseParenList

sugarParseSquareList :: Parser ScanStep
sugarParseSquareList = do
  (sl, _) <- token Token'OpenSquare
  elems <- many sugarParse
  void $ token Token'CloseSquare
  note <- sugarParseNote
  let tkn = Scan'List elems Wrap'Square note
  pure (sl, tkn)

sugarParseParenList :: Parser ScanStep
sugarParseParenList = do
  (sl, _) <- token Token'OpenParen
  elems <- many sugarParse
  void $ token Token'CloseParen
  note <- sugarParseNote
  let tkn = Scan'List elems Wrap'Paren note
  pure (sl, tkn)

sugarParseQuote :: Parser ScanStep
sugarParseQuote = do
  (sl, _) <- token Token'QuoteStart
  s <- tokenQuoteString
  void $ token Token'QuoteEnd
  note <- sugarParseNote
  let tkn = Scan'String s note
  pure (sl, tkn)

sugarParseText :: Parser ScanStep
sugarParseText = do
  (sl, _) <- token Token'StringStart
  s <- tokenString
  note <- sugarParseNote
  let tkn = Scan'String s note
  pure (sl, tkn)

tokenQuoteString :: Parser String
tokenQuoteString = Parser $ \ts -> case ts of
  [] -> (ts, Left (Nothing, "tokenQuoteString end"))
  (x:xs) -> case snd x of
    Token'QuotedString s -> (xs, Right s)
    _ -> (xs, Left (Just $ fst x, "tokenQuoteString none"))

tokenString :: Parser String
tokenString = Parser $ \ts -> case ts of
  [] -> (ts, Left (Nothing, "tokenString end"))
  (x:xs) -> case snd x of
    Token'String s -> (xs, Right s)
    _ -> (xs, Left (Just $ fst x, "tokenString none"))

token :: Token -> Parser TokenStep
token t = Parser $ \ts -> case ts of
  [] -> (ts, Left (Nothing, "token none"))
  (x:xs) -> if t == (snd x) then (xs, Right x) else (xs, Left (Just $ fst x, "token no match"))

peek :: Parser TokenStep
peek = Parser $ \ts -> case ts of
  [] -> (ts, Left (Nothing, "peek"))
  (x:_) -> (ts, Right $ x)

tryPeek :: Parser (Maybe TokenStep)
tryPeek = Parser $ \ts -> case ts of
  [] -> (ts, Right Nothing)
  (x:_) -> (ts, Right $ Just x)

nextToken :: Parser TokenStep
nextToken = Parser $ \ts -> case ts of
  [] -> ([], Left (Nothing, "nextToken"))
  (x:xs) -> (xs, Right x)

between :: Applicative m => m open -> m close -> m a -> m a
between open close p = open *> p <* close

try :: Parser a -> Parser a
try p = Parser $ \ts -> case runParser p ts of
  (_, Left a) -> (ts, Left a)
  (ts', Right b)  -> (ts', Right b)

parseError :: String -> String -> Parser a
parseError descr tag = Parser $ \ts -> case ts of
    [] -> (ts, Left $ (Nothing, msg))
    ((loc,_):_) -> (ts, Left $ (Just loc, msg))
  where
    msg = tag ++ ": " ++ descr

choice :: String -> [Parser a] -> Parser a
choice description = foldr (<|>) noMatch
  where noMatch = parseError description "no match"
