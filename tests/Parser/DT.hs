module Parser.DT (spec) where

import           Helpers

import           Test.Hspec.Megaparsec
import           Text.Megaparsec (parse)
import qualified Text.Regex.PCRE.ByteString.Utils as Regex

import           Puppet.Parser.Internal
import GHC.Base (error)

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
    "Regexp" `parsed` UDTRegexp Nothing
    let foore = case Regex.compile' Regex.compBlank Regex.execBlank "foo" of
            Right ok -> ok
            Left rr -> error (show rr)
    "Regexp[/foo/]" `parsed` UDTRegexp (Just (CompRegex "foo" foore))
    it "accepts variables" $ pendingWith "to be fixed" *> parse datatype "?" "String[$var]" `shouldParse` UDTString (Just 5) Nothing
  describe "Stdlib::" $ do
    "Stdlib::HTTPUrl" `parsed` UDTData
