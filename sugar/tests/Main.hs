{-# LANGUAGE TupleSections, DeriveGeneric, OverloadedStrings, CPP #-}
import Sugar
import qualified Test.Tasty
import Test.Tasty.Hspec
import Test.Hspec

import Sugar.Lexer
import Sugar.Parser

main :: IO ()
main = do
  test <- testSpec "sugar" spec
  Test.Tasty.defaultMain test

spec :: Spec
spec = parallel $ do
  it "unit" $ do
    let actual = map snd $ psSteps $ sugarLexerState "()"
    let expected = [Token'OpenParen, Token'CloseParen]
    actual `shouldBe` expected
  it "empty map" $ do
    let actual = map snd $ psSteps $ sugarLexerState "{}"
    let expected = [Token'OpenCurl, Token'CloseCurl]
    actual `shouldBe` expected
  --
  it "" $ do
    True `shouldBe` True

mySugar :: Sugar
mySugar = Sugar'Map [(Sugar'Text "key" Nothing, Sugar'Text "value" Nothing)] Nothing

mySugar' :: Sugar
mySugar' = Sugar'Map [
    (Sugar'Text "key" (Just "A-che-ora"), Sugar'Text "value" (Just "A che ora")),
    (Sugar'Text "anotherKey" Nothing, Sugar'Map
      [
        (Sugar'Text "a" Nothing, Sugar'Text "12345\"6789" Nothing),
        (Sugar'Text "c" Nothing, Sugar'Text "12345    6789" Nothing),
        (Sugar'Text "e" Nothing, Sugar'Text "123'456789" Nothing),
        (Sugar'Text "d" Nothing, Sugar'List ["", "", Sugar'List [Sugar'List [Sugar'List ["","",Sugar'Map [("",Sugar'Map [] Nothing),("",Sugar'Map [] Nothing)] Nothing] Wrap'Paren Nothing] Wrap'Square Nothing]Wrap'Paren Nothing] Wrap'Square Nothing)
      ]
      (Just "Hello"))
  ] Nothing

data Color
  = Color'R
  | Color'G
  | Color'B
  deriving (Show, Eq)

instance FromSugar Color where
  parseSugar (Sugar'Text a _) = case a of
    "R" -> Just Color'R
    "G" -> Just Color'G
    "B" -> Just Color'B
    _ -> Nothing
  parseSugar _ = Nothing

keyVal :: IO ()
keyVal = readSugarFromFile "keyValue.sg" >>= \(Just sg) -> prettyPrintSugarIO sg

large :: IO ()
large = readSugarFromFile "large.sg" >>= \(Just sg) -> prettyPrintSugarIO sg

mysugar :: IO ()
mysugar = readSugarFromFile "mysugar.sg" >>= \(Just sg) -> prettyPrintSugarIO sg

noline :: IO ()
noline = readSugarFromFile "noline.sg" >>= \(Just sg) -> prettyPrintSugarIO sg
