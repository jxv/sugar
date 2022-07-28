{-# LANGUAGE TupleSections, DeriveGeneric, OverloadedStrings, CPP, TemplateHaskell #-}

import qualified Test.Tasty
import Data.Text.Encoding (decodeUtf8)
import Test.Tasty.Hspec
import Test.Hspec
import Data.FileEmbed (embedFile)

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
  it "Example 01 Top Level Map" $ do
    let actual = parseSugarFromText $ decodeUtf8 $(embedFile "../examples/01_top-level-map.sg")
    let expected = Just $ Sugar'Map
          [(Sugar'Text "Lorem" Nothing,Sugar'Text "ipsum" Nothing)
          ,(Sugar'Text "dolor" Nothing,Sugar'Text "site amet" Nothing)
          ,(Sugar'Text "consectetur adipiscing" Nothing,Sugar'Text "elit" Nothing)
          ,(Sugar'Text "sed do" Nothing,Sugar'Text "eiusmod tempor" Nothing)
          ]
          Nothing
    actual `shouldBe` expected
  it "Example 02 Nested Map" $ do
    let actual = parseSugarFromText $ decodeUtf8 $(embedFile "../examples/02_nested-map.sg")
    let expected = Just $ Sugar'Map
          [(Sugar'Text "Lorem" Nothing,Sugar'Map
            [(Sugar'Text "ipsum" Nothing,Sugar'Text "dolor" Nothing)
            ,(Sugar'Text "site" Nothing,Sugar'Text "amet consectetur" Nothing)
            ,(Sugar'Text "adipiscing elit" Nothing,Sugar'Text "sed" Nothing)
            ,(Sugar'Text "do eiusmod" Nothing,Sugar'Text "tempor incididunt" Nothing)
            ] Nothing)
          ,(Sugar'Text "ut labore" Nothing,Sugar'Map
            [(Sugar'Text "et" Nothing,Sugar'Text "dolore" Nothing)
            ,(Sugar'Text "manga" Nothing,Sugar'Text "aliqua ut" Nothing)
            ,(Sugar'Text "enium ad" Nothing,Sugar'Text "minim" Nothing)
            ,(Sugar'Text "veniam quis" Nothing,Sugar'Text "nostrud exercitation" Nothing)
            ] Nothing)
          ]
          Nothing
    actual `shouldBe` expected
  it "Example 03 Top Level List" $ do
    let actual = parseSugarListFromText $ decodeUtf8 $(embedFile "../examples/03_top-level-list.sg")
    let expected = Just $ Sugar'List
          [Sugar'Text "Lorem" Nothing
          ,Sugar'Text "ipsum" Nothing
          ,Sugar'Text "dolor site" Nothing
          ,Sugar'Map
            [(Sugar'Text "amet" Nothing,Sugar'Text "consectetur" Nothing)]
            Nothing
          ]
          Wrap'Square
          Nothing
    actual `shouldBe` expected
  it "Example 04 Nest List" $ do
    let actual = parseSugarListFromText $ decodeUtf8 $(embedFile "../examples/04_nest-list.sg")
    let expected = Just $ Sugar'List
          [Sugar'List
            [Sugar'Text "Lorem" Nothing,Sugar'Text "ipsum" Nothing]
            Wrap'Square
            Nothing
          ,Sugar'List
            [Sugar'Text "ipsum" Nothing
            ,Sugar'List
              [Sugar'Text "dolor site" Nothing
              ,Sugar'Map
                [(Sugar'Text "amet" Nothing,Sugar'Text "consectetur" Nothing)] Nothing
              ]
              Wrap'Square
              Nothing
            ]
            Wrap'Square
            Nothing
          ]
          Wrap'Square
          Nothing
    actual `shouldBe` expected
  it "Example 05 Note" $ do
    let actual = parseSugarFromText $ decodeUtf8 $(embedFile "../examples/05_note.sg")
    let expected = Just $ Sugar'Map
          [(Sugar'Text "Lorem" Nothing,Sugar'Text "ipsum" (Just [Sugar'Text "dolor" Nothing]))
          ,(Sugar'Text "site" Nothing,Sugar'Text "amet" (Just [Sugar'Text "consectetur adipiscing" Nothing]))
          ,(Sugar'Map
            [(Sugar'Text "elit" Nothing,Sugar'Map
              [(Sugar'Text "sed" Nothing,Sugar'Text "do" Nothing)]
              (Just [Sugar'Text "eiusmod" Nothing]))
            ]
            (Just [Sugar'Text "tempor" Nothing])
          ,Sugar'List
            [Sugar'Text "incididunt" Nothing
            ,Sugar'Text "ut" (Just [Sugar'Text "labore" Nothing])
            ,Sugar'Text "et" (Just [Sugar'Text "dolore" Nothing,Sugar'Text "magna" Nothing])
            ]
            Wrap'Square
            (Just [Sugar'List [] Wrap'Square Nothing]))
          ]
          Nothing
    actual `shouldBe` expected
  it "Example 06 Top Level Non-Text Map" $ do
    let actual = parseSugarFromText $ decodeUtf8 $(embedFile "../examples/06_top-level-non-text-map.sg")
    let expected = Just $ Sugar'Map
          [
            (Sugar'Map
              [(Sugar'Text "first-name" Nothing,Sugar'Text "last-name" Nothing)]
              Nothing
            ,Sugar'Text "person" Nothing)
          , (Sugar'List
              [Sugar'Text "a" Nothing,Sugar'Text "b" Nothing,Sugar'Text "c" Nothing,Sugar'Text "d" Nothing,Sugar'Text "id" Nothing]
              Wrap'Square
              Nothing
            ,Sugar'Text "value" Nothing)
          ,(Sugar'Map
            [(Sugar'Text "nested" Nothing,
              Sugar'Map [(Sugar'Text "ident" Nothing,Sugar'Text "ifier" Nothing)] Nothing)] Nothing
            ,Sugar'Map
              [ (Sugar'Text "with" Nothing
                ,Sugar'Map
                  [(Sugar'Text "nested" Nothing,Sugar'Text "value-pair" Nothing)]
                  Nothing)
              ]
            Nothing)
          ]
          Nothing
    actual `shouldBe` expected
  it "Example 07 Comments" $ do
    let actual = parseSugarFromText $ decodeUtf8 $(embedFile "../examples/07_comments.sg")
    let expected = Just $ Sugar'Map
          [(Sugar'Text "a" Nothing,Sugar'Text "b" Nothing)
          ,(Sugar'Text "c" Nothing,Sugar'Text "d" Nothing)
          ,(Sugar'Text "e" Nothing,Sugar'Text "f" Nothing)
          ,(Sugar'Text "g" Nothing,Sugar'Text "h" Nothing)
          ]
          Nothing
    actual `shouldBe` expected
