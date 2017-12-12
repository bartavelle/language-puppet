module DT.Parser (spec) where

import           Helpers

import           Puppet.Parser
import           Test.Hspec.Megaparsec
import           Text.Megaparsec (parse)

spec :: Spec
spec = do
  let parsed s r = it ("accepts " <> toS s) $ parse datatype "?" s `shouldParse` r
      failed s = it ("rejects " <> toS s) $ shouldFailOn (parse datatype "?") s
  describe "String" $ do
    "String" `parsed` UDTString Nothing Nothing
    failed "String[]"
    failed "String[4,5,6]"
    "String[5]" `parsed` UDTString (Just 5) Nothing
    "String[5,8]" `parsed` UDTString (Just 5) (Just 8)
    it "accepts variables" $ pendingWith "to be fixed" *> parse datatype "?" "String[$var]" `shouldParse` UDTString (Just 5) Nothing
  describe "Stdlib::" $ do
    "Stdlib::HTTPUrl" `parsed` UDTData
