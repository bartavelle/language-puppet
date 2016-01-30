{-# LANGUAGE OverloadedLists #-}
module Function.ShellquoteSpec (spec, main) where

import           Test.Hspec

import           Data.Text (Text)
import qualified Data.Vector as V
import           Control.Monad
import           Data.Monoid

import           Puppet.Interpreter.Resolve
import           Puppet.Interpreter.Pure
import           Puppet.Interpreter.Types
import           Puppet.Parser.Types
import           Puppet.PP

main :: IO ()
main = hspec spec

evalArgs :: [Expression] -> Either PrettyError Text
evalArgs = dummyEval . resolveValue . UFunctionCall "shellquote" . V.fromList
        >=> \pv -> case pv of
                      PString s -> return s
                      _ -> Left ("Expected a string, not " <> PrettyError (pretty pv))

spec :: Spec
spec = describe "the shellquote function" $ do
    let check args res = case evalArgs args of
                             Left rr -> expectationFailure (show rr)
                             Right res' -> res' `shouldBe` res
    it "should handle no arguments" (check [] "")
    it "should handle array arguments" $
        check ["foo", ["bar@example.com", "localhost:/dev/null"], "xyzzy+-4711,23"]
              "foo bar@example.com localhost:/dev/null xyzzy+-4711,23"
    it "should quote unsafe characters" $
        check ["/etc/passwd ", "(ls)", "*", "[?]", "'&'"]
              "/etc/passwd \" \"(ls)\" \"*\" \"[?]\" \"'&'\""
    it "should deal with double quotes" $
        check ["\"foo\"bar\""]
              "'\"foo\"bar\"'"
    it "should cope with dollar signs" $
        check ["$PATH", "foo$bar", "\"x$\""]
              "'$PATH' 'foo$bar' '\"x$\"'"
    it "should deal with apostrophes (single quotes)" $
        check ["'foo'bar'", "`$'EDITOR'`"]
              "\"'foo'bar'\" \"\\`\\$'EDITOR'\\`\""
    it "should cope with grave accents (backquotes)" $
        check ["`echo *`", "`ls \"$MAILPATH\"`"]
              "'`echo *`' '`ls \"$MAILPATH\"`'"
    it "should deal with both single and double quotes" $
        check ["'foo\"bar\"xyzzy'", "\"foo'bar'xyzzy\""]
              "\"'foo\\\"bar\\\"xyzzy'\" \"\\\"foo'bar'xyzzy\\\"\""
    it "should handle multiple quotes *and* dollars and backquotes" $
        check ["'foo\"$x`bar`\"xyzzy'"]
              "\"'foo\\\"\\$x\\`bar\\`\\\"xyzzy'\""
    it "should handle linefeeds" $
        check ["foo \n bar"]
              "\"foo \n bar\""
