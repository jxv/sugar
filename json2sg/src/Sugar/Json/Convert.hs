{-# LANGUAGE OverloadedStrings #-}
module Sugar.Json.Convert
  ( main
  ) where

import qualified Data.Aeson as Json
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.IO as T
import System.IO (stdin, stderr, stdout)

import Sugar
import Sugar.Json () -- FromJSON Sugar instance

main :: IO ()
main = do
  contents <- LBS.hGetContents stdin
  case Json.decode' contents :: Maybe Sugar of
    Nothing -> T.hPutStrLn stderr "Unable to parse JSON"
    Just sg -> T.hPutStrLn stdout $ prettyPrintSugar sg
