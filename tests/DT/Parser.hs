module DT.Parser (spec) where

import qualified Data.Text as T
import           Puppet.Parser
import           Puppet.Parser.Types
import           Test.Hspec
import           Test.Hspec.Megaparsec
import           Text.Megaparsec (parse)

spec :: Spec
spec = do
  let prs s r = it s $ parse datatype "?" (T.pack s) `shouldParse` r
      fl s = it s $ shouldFailOn (parse datatype "?") (T.pack s)
  describe "String" $ do
    "String" `prs` CoreString Nothing Nothing
    fl "String[]"
    fl "String[4,5,6]"
    "String[5]" `prs` CoreString (Just 5) Nothing
    "String[5,8]" `prs` CoreString (Just 5) (Just 8)
