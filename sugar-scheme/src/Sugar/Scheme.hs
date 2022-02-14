{-# LANGUAGE OverloadedStrings #-}
module Sugar.Scheme where
  
import Sugar
import Language.Scheme.Types
import Language.Scheme.Parser
import Data.Text (unpack)
import qualified Data.Map as Map
import qualified Text.Parsec.Token as P
import Text.ParserCombinators.Parsec
import Text.Parsec.Language ()


toLispVal :: Sugar -> LispVal
toLispVal (Sugar'Unit _) = List [] -- TODO: should this be 'nil'?
toLispVal (Sugar'Text txt _) =  case parse valParser "valParser" (unpack txt) of
  Left _ -> String (unpack txt)
  Right v -> v
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
toLispVal (Sugar'List ls _ _) = List (map toLispVal ls)
toLispVal (Sugar'Map m _) = HashTable $ Map.fromList $ map (\(x,y) -> (toLispVal x, toLispVal y)) m