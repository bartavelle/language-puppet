module Function.LookupSpec (spec, main) where

import           Helpers

main :: IO ()
main = hspec spec

fname = "lookup"

checkSuccess :: [Expression] -> Text -> Expectation
checkSuccess = checkExprsSuccess fname

checkError :: [Expression] -> String -> Expectation
checkError = checkExprsError fname
 gs
spec :: Spec
spec = do
    it "should fail with no argument" (checkError [] "Expects one, two or three arguments")
    it "should succeed with one argument" (checkSuccess ["hostname"] "pure")
    it "should succeed with two arguments, the second on being a datatype" (checkSuccess ["hostname", "String"] "pure")
    it "should fail when the type mismatched" (pendingWith "Bug: lookup should fail on type mismatched" *> checkError ["hostname", "Array[String]"] "pure")
    it "should fail with two arguments both strings" (pendingWith "Bug: lookup should not accept a default as the second argument" *> checkError ["hostname", "default"] "pure")
