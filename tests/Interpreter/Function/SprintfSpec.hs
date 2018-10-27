module Interpreter.Function.SprintfSpec (spec, main) where

import           Helpers

main :: IO ()
main = hspec spec

fname = "sprintf"

checkSuccess :: [Expression] -> Text -> Expectation
checkSuccess = checkExprsSuccess fname

checkError :: [Expression] -> String -> Expectation
checkError = checkExprsError fname

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
    it "should work with one int argument" (checkSuccess ["hello %f", 1.0] "hello 1.0" )
    it "should work with one int argument" (checkSuccess ["hello %.1f", 1.23] "hello 1.2" )
    it "should pad with zeroes" (checkSuccess ["hello %03d", 10] "hello 010")
    it "should pad with spaces" (checkSuccess ["hello % 3d", 10] "hello  10")
    it "should format integers" (checkSuccess ["%+05d", 23] "+0023")
    it "should format floats" (checkSuccess ["%+.2f", 2.7182818284590451] "+2.72")
    it "should format large floats" (pendingWith "Minor formatting difference" >> checkSuccess ["%+.2e", 27182818284590451] "+2.72e+16")
    it "should work with    " (checkSuccess ["%5d"   , 5] "    5")
    it "should work with   0" (checkSuccess ["%05d"  , 5] "00005")
    it "should work with  - " (checkSuccess ["%-5d"  , 5] "5    ")
    it "should work with  -0" (checkSuccess ["%-05d" , 5] "5    ")
    it "should work with +  " (checkSuccess ["%+5d"  , 5] "   +5")
    it "should work with + 0" (checkSuccess ["%+05d" , 5] "+0005")
    it "should work with +- " (checkSuccess ["%+-5d" , 5] "+5   ")
    it "should work with +-0" (checkSuccess ["%+-05d", 5] "+5   ")
    it "should perform more complex formatting" (pendingWith "# is not yet supported" >> checkSuccess [ "<%.8s:%#5o %#8X (%-8s)>", "overlongstring", 23, 48879, "foo" ] "<overlong:  027   0XBEEF (foo     )>")
