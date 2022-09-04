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

tokenToLispVal :: P.TokenStep -> Either (SourceLocation, String) LispVal
tokenToLispVal (_, P.Unit _) = Right nullLisp
tokenToLispVal (loc, P.Text txt _) =  case parse valParser "valParser" txt of
  Left _ -> Left (loc,txt)
  Right v -> Right v
  where
    lexeme = P.lexeme lexer
    lexer = P.makeTokenParser lispDef
    valParser =
          try (lexeme parseComplexNumber)
      <|> try (lexeme parseRationalNumber)
      <|> try (lexeme parseRealNumber)
      <|> try (lexeme parseNumber)
      <|> try parseAtom
      <|> lexeme parseString
      <|> lexeme parseBool
      <|> parseQuoted
      <|> parseQuasiQuoted
      <|> parseUnquoted
tokenToLispVal (_, P.List ls _ _) = List <$> mapM tokenToLispVal ls
tokenToLispVal (_, P.Map m _) = (HashTable . Map.fromList) <$> mapM (\(x,y) -> (,) <$> tokenToLispVal x <*> tokenToLispVal y) m

sugarToLispVal :: Sugar -> Either String LispVal
sugarToLispVal (Sg.Unit _) = Right nullLisp
sugarToLispVal (Sg.Text txt _) =  case parse valParser "valParser" (unpack txt) of
  Left _ -> Left (unpack txt)
  Right v -> Right v
  where
    lexeme = P.lexeme lexer
    lexer = P.makeTokenParser lispDef
    valParser =
          try (lexeme parseComplexNumber)
      <|> try (lexeme parseRationalNumber)
      <|> try (lexeme parseRealNumber)
      <|> try (lexeme parseNumber)
      <|> try parseAtom
      <|> lexeme parseString
      <|> lexeme parseBool
      <|> parseQuoted
      <|> parseQuasiQuoted
      <|> parseUnquoted
sugarToLispVal (Sg.List ls _ _) = List <$> mapM sugarToLispVal ls
sugarToLispVal (Sg.Map m _) = (HashTable . Map.fromList) <$> mapM (\(x,y) -> (,) <$> sugarToLispVal x <*> sugarToLispVal y) m
