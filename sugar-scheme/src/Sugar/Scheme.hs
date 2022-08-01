{-# LANGUAGE OverloadedStrings #-}
module Sugar.Scheme
  ( tokenToLispVal
  , sugarToLispVal
  ) where
  
import Sugar
import Language.Scheme.Types
import Language.Scheme.Parser
import Data.Text (unpack)
import qualified Data.Map as Map
import qualified Text.Parsec.Token as P
import Text.ParserCombinators.Parsec
import Text.Parsec.Language ()

tokenToLispVal :: TokenStep -> Either (SourceLocation, String) LispVal
tokenToLispVal (_, Token'Unit _) = Right nullLisp
tokenToLispVal (loc, Token'Text txt _) =  case parse valParser "valParser" txt of
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
tokenToLispVal (_, Token'List ls _ _) = List <$> mapM tokenToLispVal ls
tokenToLispVal (_, Token'Map m _) = (HashTable . Map.fromList) <$> mapM (\(x,y) -> (,) <$> tokenToLispVal x <*> tokenToLispVal y) m

sugarToLispVal :: Sugar -> Either String LispVal
sugarToLispVal (Sugar'Unit _) = Right nullLisp
sugarToLispVal (Sugar'Text txt _) =  case parse valParser "valParser" (unpack txt) of
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
sugarToLispVal (Sugar'List ls _ _) = List <$> mapM sugarToLispVal ls
sugarToLispVal (Sugar'Map m _) = (HashTable . Map.fromList) <$> mapM (\(x,y) -> (,) <$> sugarToLispVal x <*> sugarToLispVal y) m
