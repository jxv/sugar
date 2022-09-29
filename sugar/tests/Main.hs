{-# LANGUAGE TupleSections, DeriveGeneric, OverloadedStrings, CPP, TemplateHaskell #-}

import qualified Test.Tasty
import Data.Text.Encoding (decodeUtf8)
import Test.Tasty.Hspec
import Test.Hspec
import Data.FileEmbed (embedFile)

import Sugar
import Sugar.Lexer

main :: IO ()
main = do
  test <- testSpec "sugar" spec
  Test.Tasty.defaultMain test

spec :: Spec
spec = parallel $ do
  it "unit" $ do
    let actual = map snd $ psSteps $ sugarLexerState "()"
    let expected = [OpenParen, CloseParen]
    actual `shouldBe` expected
  it "string" $ do
    let actual = map snd $ psSteps $ sugarLexerState "hello"
    let expected = [StringStart, String "hello"]
    actual `shouldBe` expected
  it "empty map" $ do
    let actual = map snd $ psSteps $ sugarLexerState "{}"
    let expected = [OpenCurl, CloseCurl]
    actual `shouldBe` expected
  it "list" $ do
    let actual = map snd $ psSteps $ sugarLexerState "[]"
    let expected = [OpenSquare, CloseSquare]
    actual `shouldBe` expected
  it "angle" $ do
    let actual = map snd $ psSteps $ sugarLexerState "<>"
    let expected = [OpenAngle, CloseAngle]
    actual `shouldBe` expected
  it "Example 01 Top Level Map" $ do
    let actual = parseSugarFromText $ decodeUtf8 $(embedFile "../examples/01_top-level-map.sg")
    let expected = Just $ Map
          [(Text "Lorem" NoQuote Nothing,Text "ipsum" NoQuote Nothing)
          ,(Text "dolor" NoQuote Nothing,Text "site amet" HasQuote Nothing)
          ,(Text "consectetur adipiscing" HasQuote Nothing,Text "elit" NoQuote Nothing)
          ,(Text "sed do" HasQuote Nothing,Text "eiusmod tempor" HasQuote Nothing)
          ]
          Nothing
    actual `shouldBe` expected
  it "Example 02 Nested Map" $ do
    let actual = parseSugarFromText $ decodeUtf8 $(embedFile "../examples/02_nested-map.sg")
    let expected = Just $ Map
          [(Text "Lorem" NoQuote Nothing,Map
            [(Text "ipsum" NoQuote Nothing,Text "dolor" NoQuote Nothing)
            ,(Text "site" NoQuote Nothing,Text "amet consectetur" HasQuote Nothing)
            ,(Text "adipiscing elit" HasQuote Nothing,Text "sed" NoQuote Nothing)
            ,(Text "do eiusmod" HasQuote Nothing,Text "tempor incididunt" HasQuote Nothing)
            ] Nothing)
          ,(Text "ut labore" HasQuote Nothing,Map
            [(Text "et" NoQuote Nothing,Text "dolore" NoQuote Nothing)
            ,(Text "manga" NoQuote Nothing,Text "aliqua ut" HasQuote Nothing)
            ,(Text "enium ad" HasQuote Nothing,Text "minim" NoQuote Nothing)
            ,(Text "veniam quis" HasQuote Nothing,Text "nostrud exercitation" HasQuote Nothing)
            ] Nothing)
          ]
          Nothing
    actual `shouldBe` expected
  it "Example 03 Top Level List" $ do
    let actual = parseSugarListFromText $ decodeUtf8 $(embedFile "../examples/03_top-level-list.sg")
    let expected = Just $ List
          [Text "Lorem" NoQuote Nothing
          ,Text "ipsum" HasQuote Nothing
          ,Text "dolor site" HasQuote Nothing
          ,Map
            [(Text "amet" NoQuote Nothing,Text "consectetur" NoQuote Nothing)]
            Nothing
          ]
          Square
          Nothing
    actual `shouldBe` expected
  it "Example 04 Nest List" $ do
    let actual = parseSugarListFromText $ decodeUtf8 $(embedFile "../examples/04_nest-list.sg")
    let expected = Just $ List
          [List
            [Text "Lorem" NoQuote Nothing,Text "ipsum" NoQuote Nothing]
            Square
            Nothing
          ,List
            [Text "ipsum" HasQuote Nothing
            ,List
              [Text "dolor site" HasQuote Nothing
              ,Map
                [(Text "amet" NoQuote Nothing,Text "consectetur" NoQuote Nothing)] Nothing
              ]
              Square
              Nothing
            ]
            Square
            Nothing
          ,List 
            [Text "x" NoQuote Nothing
            ,Text "y" NoQuote Nothing
            ]
            Paren
            Nothing
          ]
          Square
          Nothing
    actual `shouldBe` expected
  it "Example 05 Note" $ do
    let actual = parseSugarFromText $ decodeUtf8 $(embedFile "../examples/05_note.sg")
    let expected = Just $ Map
          [(Text "Lorem" NoQuote Nothing,Text "ipsum" NoQuote (Just [Text "dolor" NoQuote Nothing]))
          ,(Text "site" NoQuote Nothing,Text "amet" NoQuote (Just [Text "consectetur adipiscing" HasQuote Nothing]))
          ,(Map
            [(Text "elit" NoQuote Nothing,Map
              [(Text "sed" NoQuote Nothing,Text "do" NoQuote Nothing)]
              (Just [Text "eiusmod" NoQuote Nothing]))
            ]
            (Just [Text "tempor" NoQuote Nothing])
          ,List
            [Text "incididunt" NoQuote Nothing
            ,Text "ut" NoQuote (Just [Text "labore" NoQuote Nothing])
            ,Text "et" NoQuote (Just [Text "dolore" NoQuote Nothing,Text "magna" NoQuote Nothing])
            ]
            Square
            (Just [List [] Square Nothing]))
          ,(Unit Nothing
           ,List
            [Text "incididunt" NoQuote Nothing
            ,Text "ut" NoQuote (Just [Text "labore" NoQuote Nothing])
            ,Text "et" NoQuote (Just [Text "dolore" NoQuote Nothing,Text "magna" NoQuote Nothing])
            ]
            Square
            (Just [Text "a" NoQuote Nothing,Text "b" NoQuote Nothing,Text "c" NoQuote Nothing]))
          ,(Unit Nothing
           ,List
            [Text "incididunt" NoQuote Nothing
            ,Text "ut" NoQuote (Just [Text "labore" NoQuote Nothing])
            ,Text "et" NoQuote (Just [Text "dolore" NoQuote Nothing,Text "magna" NoQuote Nothing])
            ]
            Square
            (Just [Text "a" NoQuote Nothing,Text "b" NoQuote Nothing,Text "c" NoQuote Nothing]))
          ,(Unit Nothing
           ,List
            [Text "incididunt" NoQuote Nothing
            ,Text "ut" NoQuote (Just [Text "labore" NoQuote Nothing])
            ,Text "et" NoQuote (Just [Text "dolore" NoQuote Nothing,Text "magna" NoQuote Nothing])
            ]
            Square
            (Just [Text "a" NoQuote Nothing,Text "b" NoQuote Nothing,Text "c" NoQuote Nothing]))
          ]
          Nothing
    actual `shouldBe` expected
  it "Example 06 Top Level Non-Text Map" $ do
    let actual = parseSugarFromText $ decodeUtf8 $(embedFile "../examples/06_top-level-non-text-map.sg")
    let expected = Just $ Map
          [
            (Map
              [(Text "first-name" NoQuote Nothing,Text "last-name" NoQuote Nothing)]
              Nothing
            ,Text "person" NoQuote Nothing)
          , (List
              [Text "a" NoQuote Nothing,Text "b" NoQuote Nothing,Text "c" NoQuote  Nothing,Text "d" NoQuote Nothing,Text "id" NoQuote Nothing]
              Square
              Nothing
            ,Text "value" NoQuote Nothing)
          ,(Map
            [(Text "nested" NoQuote Nothing,
              Map [(Text "ident" NoQuote Nothing,Text "ifier" NoQuote Nothing)] Nothing)] Nothing
            ,Map
              [ (Text "with" NoQuote Nothing
                ,Map
                  [(Text "nested" NoQuote Nothing,Text "value-pair" NoQuote Nothing)]
                  Nothing)
              ]
            Nothing)
          ]
          Nothing
    actual `shouldBe` expected
  it "Example 07 Comments" $ do
    let actual = parseSugarFromText $ decodeUtf8 $(embedFile "../examples/07_comments.sg")
    let expected = Just $ Map
          [(Text "a" NoQuote Nothing,Text "b" NoQuote Nothing)
          ,(Text "c" NoQuote Nothing,Text "d" NoQuote Nothing)
          ,(Text "e" NoQuote Nothing,Text "f" NoQuote Nothing)
          ,(Text "g" NoQuote Nothing,Text "h" NoQuote Nothing)
          ]
          Nothing
    actual `shouldBe` expected
