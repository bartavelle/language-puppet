{-# LANGUAGE OverloadedLists #-}
module Interpreter.Function.ShellquoteSpec (spec, main) where

import           Helpers

main :: IO ()
main = hspec spec

check :: [Expression] -> Text -> Expectation
check = checkExprsSuccess "shellquote"

spec :: Spec
spec = do
    it "should handle no arguments" (check [] "")
    it "should handle array arguments" $
        check ["foo", ["bar@example.com", "localhost:/dev/null"], "xyzzy+-4711,23"]
              "foo bar@example.com localhost:/dev/null xyzzy+-4711,23"
    it "should quote unsafe characters" $
        check ["/etc/passwd ", "(ls)", "*", "[?]", "'&'"]
              "\"/etc/passwd \" \"(ls)\" \"*\" \"[?]\" \"'&'\""
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
