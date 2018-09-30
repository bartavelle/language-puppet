module LexerSpec(spec) where

import           XPrelude

import           System.FilePath.Glob
import           Test.Hspec
import           Text.Megaparsec      (eof, parse)
import           Test.Hspec.Megaparsec

import           Puppet.Parser
import           Puppet.Parser.Internal

validFiles = do
  files <- runIO $ globDir1 (compile "*.pp") "tests/lexer"
  mapM_ test files

  where
    test fp = do
      r <- runIO  $ fmap check (readFile fp)
      it ("should parse " <> fp) r
    check i  =
      parse (puppetParser <* eof) empty `shouldSucceedOn` i

invalidResourceReference =
  invalidTest "should fail to parse resource reference with a space after the resource type" resourceReference  "File ['/test']"

spec = describe "Lexer" $ do
  describe "Valid lexer" $ validFiles
  describe "Invalid lexer" $ do
    invalidResourceReference

-- Utils
invalidTest msg p s = it msg $ parse (p <* eof) mempty `shouldFailOn` s
