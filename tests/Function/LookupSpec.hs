module Function.LookupSpec (spec, main) where

import           Helpers

main :: IO ()
main = hspec spec

fname = "lookup"
expectedErrMsg = "Wrong set of arguments"

checkSuccess :: [Expression] -> Text -> Expectation
checkSuccess = checkExprsSuccess fname

checkError :: [Expression] -> String -> Expectation
checkError = checkExprsError fname

spec :: Spec
spec = do
    it "should fail with no argument" (checkError [] expectedErrMsg)
    it "should succeed with one argument" (checkSuccess ["hostname"] "pure")
    it "should succeed with two arguments, the second on being a datatype" (pendingWith "Bug: lookup should work with the second arg being a datatype " *> checkSuccess ["hostname", "String"] "pure")
    it "should fail when the type mismatched" (pendingWith "Bug: lookup should fail on type mismatched" *> checkError ["hostname", "Array[String]"] "pure")
    it "should fail with two arguments both strings" (checkError ["hostname", "default"] expectedErrMsg)
