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
    it "should fail if arg is not provided" (checkError ["hello %s"] "not enough arguments")
    it "should fail when a wrong format instruction is used" (checkError ["hello %d", "world"] "Don't know how to convert this to a number" )
    it "should fail when a wrong format instruction is used" (checkError ["hello %f", "world"] "Don't know how to convert this to a number" )
    it "should work with one int argument" (pendingWith "Does not work with floating number" >> checkSuccess ["hello %f", 1.0] "hello 1.0" )
    it "should work with one int argument" (pendingWith "Does not work with floating number" >> checkSuccess ["hello %.1f", 1.23] "hello 1.2" )
    it "should pad with zeroes" (pendingWith "Doesn't support format modifiers" >> checkSuccess ["hello %03d", 10] "hello 010")
    it "should pad with spaces" (pendingWith "Doesn't support format modifiers" >> checkSuccess ["hello % 3d", 10] "hello  10")
    it "should format integers" (pendingWith "Doesn't support format modifiers" >> checkSuccess ["%+05d", 23] "+0023")
    it "should format floats" (pendingWith "Doesn't support format modifiers" >> checkSuccess ["%+.2f", 2.7182818284590451] "+2.72")
    it "should format large floats" (pendingWith "Doesn't support format modifiers" >> checkSuccess ["%+.2e", 27182818284590451] "+2.72e+16")
    it "should perform more complex formatting" (pendingWith "Doesn't support format modifiers" >> checkSuccess [ "<%.8s:%#5o %#8X (%-8s)>", "overlongstring", 23, 48879, "foo" ] "<overlong:  027   0XBEEF (foo     )>")
