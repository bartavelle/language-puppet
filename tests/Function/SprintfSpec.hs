{-# LANGUAGE OverloadedLists #-}
module Function.SprintfSpec (spec, main) where

import           Test.Hspec
import           Data.Text (Text)
import           Control.Monad
import qualified Data.Vector as V
import           Data.Monoid

import           Puppet.Interpreter.Pure
import           Puppet.Interpreter.Resolve
import           Puppet.Interpreter.Types
import           Puppet.Parser.Types
import           Puppet.PP

main :: IO ()
main = hspec spec

evalArgs :: [Expression] -> Either PrettyError Text
evalArgs = dummyEval . resolveValue . UFunctionCall "sprintf" . V.fromList
  >=> \pv -> case pv of
                PString s -> return s
                _ -> Left ("Expected a string, not " <> PrettyError (pretty pv))

checkSuccess :: [Expression] -> Text -> Expectation
checkSuccess args res =
  case evalArgs args of
    Left rr -> expectationFailure (show rr)
    Right res' -> res' `shouldBe` res
checkError :: [Expression] -> [Char] -> Expectation
checkError args ins =
  case evalArgs args of
    Left rr -> show rr `shouldContain` ins
    Right r -> expectationFailure ("Should have errored, received this instead: " <> show r)

spec :: Spec
spec = do
    it "should fail with no argument" (checkError [] "at least two arguments")
    it "should fail with more than one argument" (checkError ["hello", 1, 2] "not yet implemented")
    it "should work with one string argument" (checkSuccess ["hello %s", "world"] "hello world" )
    it "should fail when a wrong format instruction is used" (pendingWith "? catchError not firing ?" >> checkError ["hello %d", "world"] "arg(s) invalid" )
    it "should work with one int argument" (pendingWith "not implemented" >> checkSuccess ["hello %d", 1] "hello 1" )
