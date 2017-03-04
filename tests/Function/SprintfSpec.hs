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
checkError :: [Expression] -> String -> Expectation
checkError args msg =
  case evalArgs args of
    Left rr -> show rr `shouldContain` msg
    Right r -> expectationFailure ("Should have errored, received this instead: " <> show r)

spec :: Spec
spec = do
    it "should fail with no argument" (checkError [] "Expects a string as its first argument")
    it "should succeed with one argument" (checkSuccess ["hello"] "hello") -- puppet sprintf accepts one arg
    it "should work with multiple arguments" (checkSuccess ["hello %s %s", "world", "!"] "hello world !")
    it "should work with one string argument" (checkSuccess ["hello %s", "world"] "hello world" )
    it "should work with one int argument" (checkSuccess ["hello %d", 10] "hello 10" )
    it "should fail if arg is not provided" (checkError ["hello %s"] "invalid arg(s)")
    it "should fail when a wrong format instruction is used" (checkError ["hello %d", "world"] "Don't know how to convert this to a number" )
    it "should fail when a wrong format instruction is used" (checkError ["hello %f", "world"] "Don't know how to convert this to a number" )
    it "should work with one int argument" (pendingWith "Does not work with floating number" >> checkSuccess ["hello %f", 1.0] "hello 1.0" )
    it "should work with one int argument" (pendingWith "Does not work with floating number" >> checkSuccess ["hello %.1f", 1.23] "hello 1.2" )
