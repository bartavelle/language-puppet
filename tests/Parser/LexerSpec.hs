module Parser.LexerSpec (spec) where

import Puppet.Parser
import Puppet.Parser.Internal
import System.FilePath.Glob
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec (eof, parse)
import XPrelude

validFiles = do
  files <- runIO $ globDir1 (compile "*.pp") "tests/Parser/lexer"
  mapM_ test files
  where
    test fp = do
      r <- runIO $ fmap check (readFile fp)
      it ("should parse " <> fp) r
    check i =
      parse (puppetParser <* eof) empty `shouldSucceedOn` i

spec = describe "Lexer" $ do
  describe "Valid lexer" validFiles
  describe "Invalid lexer" $ do
    it "should fail to parse resource reference with a space after the resource type" $ invalid resourceReference "File ['/test']"
    xit "should fail if there is a space after the variable name" $ invalid interpolableString "\"${os ['name']}\""

-- Utils
invalid p s = parse (p <* eof) mempty `shouldFailOn` s
