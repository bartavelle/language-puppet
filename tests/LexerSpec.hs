module LexerSpec(spec) where

import           System.FilePath.Glob
import           Test.Hspec
import           Text.Megaparsec      (eof, parse)
import           Test.Hspec.Megaparsec
import           XPrelude

import           Puppet.Parser

alltests = do
  files <- runIO $ globDir1 (compile "*.pp") "tests/lexer"
  mapM_ test files

  where
    test fp = do
      r <- runIO  $ fmap check (readFile fp)
      it ("should parse " <> fp) r
    check i  =
      parse (puppetParser <* eof) empty `shouldSucceedOn` i

spec = describe "Lexer" $ alltests
