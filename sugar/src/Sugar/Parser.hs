{-# LANGUAGE TupleSections, DeriveGeneric, OverloadedStrings, CPP, NamedFieldPuns #-}
module Sugar.Parser
  ( Token(..)
  , TokenStep
  , TokenNote
  , ParseError
  , Parser(..)
  , flatten
  , sugarParse
  , sugarParseTopLevel
  , sugarParseMap
  , sugarParseList
  ) where

import Control.Monad
import Control.Applicative

import qualified Data.Text as T
import qualified Sugar.Types as Sg

import Sugar.Types (Wrap(..))
import Sugar.Lexer

data Token
  = Unit TokenNote
  | Text String TokenNote
  | List [TokenStep] Wrap TokenNote
  | Map [(TokenStep,TokenStep)] TokenNote
  deriving (Show, Eq)

type TokenStep = (SourceLocation, Token)
type TokenNote = Maybe [TokenStep]
type ParseError = (Maybe SourceLocation, String)

newtype Parser a = Parser
  { runParser :: [LexemeStep] -> ([LexemeStep], Either ParseError a) }

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

flatten :: TokenStep -> Sg.Sugar
flatten (_, s) = case s of
  Unit note -> Sg.Unit (fmap flatten <$> note)
  Text str note -> Sg.Text (T.pack str) (fmap flatten <$> note)
  List elems wrap note -> Sg.List (flatten <$> elems) wrap (fmap flatten <$> note)
  Map elems note -> Sg.Map ((\(x,y) -> (flatten x, flatten y)) <$> elems) (fmap flatten <$> note)

--

sugarParse :: Parser TokenStep
sugarParse = do
  (loc, tkn) <- peek
  case tkn of
    Start -> sugarParse
    OpenCurl -> sugarParseMap
    OpenParen -> try sugarParseUnit <|> sugarParseParenList
    OpenSquare -> sugarParseSquareList
    QuoteStart -> sugarParseQuote
    StringStart -> sugarParseText
    SingleLineComment -> nextLexeme *> ignoreUntilNewLine loc *> sugarParse
    MultiLineCommentStart -> nextLexeme *> ignoreUntilMultilineCommentEnd 0 *> sugarParse
    _ -> sugarParseUnexpected (loc, tkn)

ignoreUntilNewLine :: SourceLocation -> Parser ()
ignoreUntilNewLine sl = do
  tkn' <- tryPeek
  case tkn' of
    Nothing -> return ()
    Just (loc,_) -> if slLine loc == slLine sl
      then nextLexeme *> ignoreUntilNewLine loc
      else return ()

ignoreUntilMultilineCommentEnd :: Int -> Parser ()
ignoreUntilMultilineCommentEnd nested = do
  tkn' <- tryPeek
  case tkn' of
    Nothing -> sugarParseExpected "`|#` to close multi-line comment"
    Just (_,MultiLineCommentStart) -> nextLexeme *> ignoreUntilMultilineCommentEnd (nested + 1)
    Just (_,MultiLineCommentEnd) -> do
      void nextLexeme
      when (nested > 0) $ ignoreUntilMultilineCommentEnd (nested - 1)
    Just (_,_) -> nextLexeme *> ignoreUntilMultilineCommentEnd nested

sugarParseUnexpected :: LexemeStep -> Parser TokenStep
sugarParseUnexpected (loc, tkn) = Parser $ \ts -> (ts, Left (Just loc, "Unexpected: " ++ show tkn))

sugarParseExpected :: String -> Parser ()
sugarParseExpected expected =  Parser $ \ts -> (ts, Left (Nothing, "Expected: " ++ expected))

sugarParseNote :: Parser (Maybe [TokenStep])
sugarParseNote = do
  tkn' <- tryPeek
  case tkn' of
    Nothing -> return Nothing
    Just (_,tkn) -> case tkn of
      OpenAngle -> fmap pure $ between (lexeme OpenAngle) (lexeme CloseAngle) (many (sugarParse <* optional (try sugarParseComma)))
      _ -> pure Nothing

sugarParseUnit :: Parser TokenStep
sugarParseUnit = do
  (sl,  _) <- lexeme OpenParen
  (sl', _) <- lexeme CloseParen
  if slColumn sl + 1 ==  slColumn sl' -- no space between parens
    then do
      note <- sugarParseNote
      let tkn = Unit note
      pure (sl, tkn)
    else
      empty

sugarParseTopLevel :: Parser TokenStep
sugarParseTopLevel = sugarParseTopLevelMap

sugarParseMap :: Parser TokenStep
sugarParseMap = do
  (sl, _) <- lexeme OpenCurl
  -- elems <- many ((,) <$> sugarParse <*> (sugarParse <* optional (try sugarParseComma))) -- No colon
  elems <- many ((,) <$> (sugarParse <* sugarParseColon) <*> (sugarParse <* optional (try sugarParseComma))) -- Required colon
  -- elems <- many ((,) <$> (sugarParse <* optional (try sugarParseColon)) <*> (sugarParse <* optional (try sugarParseComma))) -- Optional colon
  void $ lexeme CloseCurl
  note <- sugarParseNote
  let tkn = Map elems note
  pure (sl, tkn)

sugarParseTopLevelMap :: Parser TokenStep
sugarParseTopLevelMap = do
  -- elems <- many ((,) <$> sugarParse <*> (sugarParse <* optional (try sugarParseComma))) -- No colon
  elems <- many ((,) <$> (sugarParse <* sugarParseColon) <*> (sugarParse <* optional (try sugarParseComma))) -- Required colon
  -- elems <- many ((,) <$> (sugarParse <* optional (try sugarParseColon)) <*> (sugarParse <* optional (try sugarParseComma))) -- Optional colon
  let tkn = Map elems Nothing
  case elems of
    (((sl,_), _):_) -> return (sl, tkn)
    [] -> return (SourceLocation 0 0, tkn)

sugarParseList :: Parser TokenStep
sugarParseList = try sugarParseSquareList <|> sugarParseParenList

sugarParseSquareList :: Parser TokenStep
sugarParseSquareList = do
  (sl, _) <- lexeme OpenSquare
  elems <- many (sugarParse <* optional (try sugarParseComma))
  void $ lexeme CloseSquare
  note <- sugarParseNote
  let tkn = List elems Square note
  pure (sl, tkn)

sugarParseParenList :: Parser TokenStep
sugarParseParenList = do
  (sl, _) <- lexeme OpenParen
  elems <- many (sugarParse <* optional sugarParseComma)
  void $ lexeme CloseParen
  note <- sugarParseNote
  let tkn = List elems Paren note
  pure (sl, tkn)

sugarParseQuote :: Parser TokenStep
sugarParseQuote = do
  (sl, _) <- lexeme QuoteStart
  s <- lexemeQuoteString
  void $ lexeme QuoteEnd
  note <- sugarParseNote
  let tkn = Text s note
  pure (sl, tkn)

sugarParseText :: Parser TokenStep
sugarParseText = do
  (sl, _) <- lexeme StringStart
  s <- lexemeString
  note <- sugarParseNote
  let tkn = Text s note
  pure (sl, tkn)

sugarParseComma :: Parser ()
sugarParseComma = do
  _ <- lexeme Comma
  pure ()

sugarParseColon :: Parser ()
sugarParseColon = do
  _ <- lexeme Colon
  pure ()

lexemeQuoteString :: Parser String
lexemeQuoteString = Parser $ \ts -> case ts of
  [] -> (ts, Left (Nothing, "lexemeQuoteString end"))
  (x:xs) -> case snd x of
    QuotedString s -> (xs, Right s)
    _ -> (xs, Left (Just $ fst x, "lexemeQuoteString none"))

lexemeString :: Parser String
lexemeString = Parser $ \ts -> case ts of
  [] -> (ts, Left (Nothing, "lexemeString end"))
  (x:xs) -> case snd x of
    String s -> (xs, Right s)
    _ -> (xs, Left (Just $ fst x, "lexemeString none"))

lexeme :: Lexeme -> Parser LexemeStep
lexeme t = Parser $ \ts -> case ts of
  [] -> (ts, Left (Nothing, "lexeme none"))
  (x:xs) -> if t == (snd x) then (xs, Right x) else (xs, Left (Just $ fst x, "lexeme no match"))

peek :: Parser LexemeStep
peek = Parser $ \ts -> case ts of
  [] -> (ts, Left (Nothing, "peek"))
  (x:_) -> (ts, Right $ x)

tryPeek :: Parser (Maybe LexemeStep)
tryPeek = Parser $ \ts -> case ts of
  [] -> (ts, Right Nothing)
  (x:_) -> (ts, Right $ Just x)

nextLexeme :: Parser LexemeStep
nextLexeme = Parser $ \ts -> case ts of
  [] -> ([], Left (Nothing, "nextLexeme"))
  (x:xs) -> (xs, Right x)

between :: Applicative m => m open -> m close -> m a -> m a
between open close p = open *> p <* close

try :: Parser a -> Parser a
try p = Parser $ \ts -> case runParser p ts of
  (_, Left a) -> (ts, Left a)
  (ts', Right b)  -> (ts', Right b)

