{-# LANGUAGE OverloadedStrings #-}
module Sugar.Scheme
  ( tokenToLispVal
  , sugarToLispVal
  ) where
  
import Sugar (Sugar)
import Sugar.Lexer (SourceLocation)
import qualified Sugar.Types as Sg
import qualified Sugar.Parser as P
import Language.Scheme.Types
import Language.Scheme.Parser
import Data.Text (unpack)
import qualified Data.Map as Map
import qualified Text.Parsec.Token as P
import Text.ParserCombinators.Parsec
import Text.Parsec.Language ()
import Text.Parsec (Parsec)
import Text.Parsec.Token (TokenParser)

tokenToLispVal :: P.TokenStep -> Either (SourceLocation, String) LispVal
tokenToLispVal (_, P.Unit _) = Right nullLisp
tokenToLispVal (_, P.Text txt Sg.HasQuote _) = pure $ String txt
tokenToLispVal (loc, P.Text txt Sg.NoQuote _) =  case parse valParser "valParser" txt of
  Left _ -> Left (loc,txt)
  Right v -> Right v
tokenToLispVal (_, P.List ls _ _) = List <$> mapM tokenToLispVal ls
tokenToLispVal (_, P.Map m _) = (HashTable . Map.fromList) <$> mapM (\(x,y) -> (,) <$> tokenToLispVal x <*> tokenToLispVal y) m

sugarToLispVal :: Sugar -> Either String LispVal
sugarToLispVal (Sg.Unit _) = Right nullLisp
sugarToLispVal (Sg.Text txt Sg.HasQuote _) = pure $ String (unpack txt)
sugarToLispVal (Sg.Text txt Sg.NoQuote _) =  case parse valParser "valParser" (unpack txt) of
  Left _ -> Left (unpack txt)
  Right v -> Right v
sugarToLispVal (Sg.List ls _ _) = List <$> mapM sugarToLispVal ls
sugarToLispVal (Sg.Map m _) = (HashTable . Map.fromList) <$> mapM (\(x,y) -> (,) <$> sugarToLispVal x <*> sugarToLispVal y) m

lexeme :: Parsec String () a -> Parsec String () a
lexeme = P.lexeme lexer

lexer :: TokenParser ()
lexer = P.makeTokenParser lispDef

valParser :: Parsec String () LispVal
valParser =
  try (lexeme parseComplexNumber)
  <|> try (lexeme parseRationalNumber)
  <|> try (lexeme parseRealNumber)
  <|> try (lexeme parseNumber)
  <|> try parseAtom
  <|> lexeme parseBool
  <|> parseQuoted
  <|> parseQuasiQuoted
  <|> parseUnquoted

