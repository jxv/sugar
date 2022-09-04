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
          [(Text "Lorem" Nothing,Text "ipsum" Nothing)
          ,(Text "dolor" Nothing,Text "site amet" Nothing)
          ,(Text "consectetur adipiscing" Nothing,Text "elit" Nothing)
          ,(Text "sed do" Nothing,Text "eiusmod tempor" Nothing)
          ]
          Nothing
    actual `shouldBe` expected
  it "Example 02 Nested Map" $ do
    let actual = parseSugarFromText $ decodeUtf8 $(embedFile "../examples/02_nested-map.sg")
    let expected = Just $ Map
          [(Text "Lorem" Nothing,Map
            [(Text "ipsum" Nothing,Text "dolor" Nothing)
            ,(Text "site" Nothing,Text "amet consectetur" Nothing)
            ,(Text "adipiscing elit" Nothing,Text "sed" Nothing)
            ,(Text "do eiusmod" Nothing,Text "tempor incididunt" Nothing)
            ] Nothing)
          ,(Text "ut labore" Nothing,Map
            [(Text "et" Nothing,Text "dolore" Nothing)
            ,(Text "manga" Nothing,Text "aliqua ut" Nothing)
            ,(Text "enium ad" Nothing,Text "minim" Nothing)
            ,(Text "veniam quis" Nothing,Text "nostrud exercitation" Nothing)
            ] Nothing)
          ]
          Nothing
    actual `shouldBe` expected
  it "Example 03 Top Level List" $ do
    let actual = parseSugarListFromText $ decodeUtf8 $(embedFile "../examples/03_top-level-list.sg")
    let expected = Just $ List
          [Text "Lorem" Nothing
          ,Text "ipsum" Nothing
          ,Text "dolor site" Nothing
          ,Map
            [(Text "amet" Nothing,Text "consectetur" Nothing)]
            Nothing
          ]
          Square
          Nothing
    actual `shouldBe` expected
  it "Example 04 Nest List" $ do
    let actual = parseSugarListFromText $ decodeUtf8 $(embedFile "../examples/04_nest-list.sg")
    let expected = Just $ List
          [List
            [Text "Lorem" Nothing,Text "ipsum" Nothing]
            Square
            Nothing
          ,List
            [Text "ipsum" Nothing
            ,List
              [Text "dolor site" Nothing
              ,Map
                [(Text "amet" Nothing,Text "consectetur" Nothing)] Nothing
              ]
              Square
              Nothing
            ]
            Square
            Nothing
          ]
          Square
          Nothing
    actual `shouldBe` expected
  it "Example 05 Note" $ do
    let actual = parseSugarFromText $ decodeUtf8 $(embedFile "../examples/05_note.sg")
    let expected = Just $ Map
          [(Text "Lorem" Nothing,Text "ipsum" (Just [Text "dolor" Nothing]))
          ,(Text "site" Nothing,Text "amet" (Just [Text "consectetur adipiscing" Nothing]))
          ,(Map
            [(Text "elit" Nothing,Map
              [(Text "sed" Nothing,Text "do" Nothing)]
              (Just [Text "eiusmod" Nothing]))
            ]
            (Just [Text "tempor" Nothing])
          ,List
            [Text "incididunt" Nothing
            ,Text "ut" (Just [Text "labore" Nothing])
            ,Text "et" (Just [Text "dolore" Nothing,Text "magna" Nothing])
            ]
            Square
            (Just [List [] Square Nothing]))
          ,(Unit Nothing
           ,List
            [Text "incididunt" Nothing
            ,Text "ut" (Just [Text "labore" Nothing])
            ,Text "et" (Just [Text "dolore" Nothing,Text "magna" Nothing])
            ]
            Square
            (Just [Text "a" Nothing,Text "b" Nothing,Text "c" Nothing]))
          ,(Unit Nothing
           ,List
            [Text "incididunt" Nothing
            ,Text "ut" (Just [Text "labore" Nothing])
            ,Text "et" (Just [Text "dolore" Nothing,Text "magna" Nothing])
            ]
            Square
            (Just [Text "a" Nothing,Text "b" Nothing,Text "c" Nothing]))
          ,(Unit Nothing
           ,List
            [Text "incididunt" Nothing
            ,Text "ut" (Just [Text "labore" Nothing])
            ,Text "et" (Just [Text "dolore" Nothing,Text "magna" Nothing])
            ]
            Square
            (Just [Text "a" Nothing,Text "b" Nothing,Text "c" Nothing]))
          ]
          Nothing
    actual `shouldBe` expected
  it "Example 06 Top Level Non-Text Map" $ do
    let actual = parseSugarFromText $ decodeUtf8 $(embedFile "../examples/06_top-level-non-text-map.sg")
    let expected = Just $ Map
          [
            (Map
              [(Text "first-name" Nothing,Text "last-name" Nothing)]
              Nothing
            ,Text "person" Nothing)
          , (List
              [Text "a" Nothing,Text "b" Nothing,Text "c" Nothing,Text "d" Nothing,Text "id" Nothing]
              Square
              Nothing
            ,Text "value" Nothing)
          ,(Map
            [(Text "nested" Nothing,
              Map [(Text "ident" Nothing,Text "ifier" Nothing)] Nothing)] Nothing
            ,Map
              [ (Text "with" Nothing
                ,Map
                  [(Text "nested" Nothing,Text "value-pair" Nothing)]
                  Nothing)
              ]
            Nothing)
          ]
          Nothing
    actual `shouldBe` expected
  it "Example 07 Comments" $ do
    let actual = parseSugarFromText $ decodeUtf8 $(embedFile "../examples/07_comments.sg")
    let expected = Just $ Map
          [(Text "a" Nothing,Text "b" Nothing)
          ,(Text "c" Nothing,Text "d" Nothing)
          ,(Text "e" Nothing,Text "f" Nothing)
          ,(Text "g" Nothing,Text "h" Nothing)
          ]
          Nothing
    actual `shouldBe` expected
