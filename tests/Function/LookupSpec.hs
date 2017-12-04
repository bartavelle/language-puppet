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
    it "should succeed with two arguments, the second on being a datatype" (checkSuccess ["hostname", Terminal (UDataType (DTString Nothing Nothing))] "pure")
    it "should fail when the type mismatched" (checkError ["hostname", Terminal (UDataType DTBoolean)] "Datatype mismatched")
    it "should fail with two arguments both strings" (checkError ["hostname", "default"] expectedErrMsg)
