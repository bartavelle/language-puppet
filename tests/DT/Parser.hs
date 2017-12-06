module DT.Parser (spec) where

import           Helpers

import           Puppet.Parser
import           Test.Hspec.Megaparsec
import           Text.Megaparsec (parse)

spec :: Spec
spec = do
  let parsed s r = it s $ parse datatype "?" (toS s) `shouldParse` r
      failed s = it s $ shouldFailOn (parse datatype "?") (toS s)
  describe "String" $ do
    "String" `parsed` DTString Nothing Nothing
    failed "String[]"
    failed "String[4,5,6]"
    "String[5]" `parsed` DTString (Just 5) Nothing
    "String[5,8]" `parsed` DTString (Just 5) (Just 8)
  describe "Stdlib::" $ do
    "Stdlib::HTTPUrl" `parsed` DTData
