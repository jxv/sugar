{-# LANGUAGE TupleSections, DeriveGeneric, OverloadedStrings, CPP #-}

import qualified Test.Tasty
import Test.Tasty.Hspec
import Test.Hspec

import Sugar

main :: IO ()
main = do
  test <- testSpec "sugar" spec
  Test.Tasty.defaultMain test

spec :: Spec
spec = parallel $ do
  it "unit" $ do
    let actual = map snd $ psSteps $ sugarLexerState "()"
    let expected = [Lexeme'OpenParen, Lexeme'CloseParen]
    actual `shouldBe` expected
  it "string" $ do
    let actual = map snd $ psSteps $ sugarLexerState "hello"
    let expected = [Lexeme'StringStart, Lexeme'String "hello"]
    actual `shouldBe` expected
  it "empty map" $ do
    let actual = map snd $ psSteps $ sugarLexerState "{}"
    let expected = [Lexeme'OpenCurl, Lexeme'CloseCurl]
    actual `shouldBe` expected
  it "list" $ do
    let actual = map snd $ psSteps $ sugarLexerState "[]"
    let expected = [Lexeme'OpenSquare, Lexeme'CloseSquare]
    actual `shouldBe` expected
  it "angle" $ do
    let actual = map snd $ psSteps $ sugarLexerState "<>"
    let expected = [Lexeme'OpenAngle, Lexeme'CloseAngle]
    actual `shouldBe` expected
