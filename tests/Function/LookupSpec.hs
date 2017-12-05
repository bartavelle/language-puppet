module Function.LookupSpec (spec, main) where

import           Helpers

main :: IO ()
main = hspec spec

fname = "lookup"
expectedErrMsg = "Wrong set of arguments"
expectedValue = "pure"

checkSuccess :: [Expression] -> Text -> Expectation
checkSuccess = checkExprsSuccess fname

checkError :: [Expression] -> String -> Expectation
checkError = checkExprsError fname


boolDatatype = Terminal (UDataType DTBoolean)
stringDatatype = Terminal (UDataType (DTString Nothing Nothing))

spec :: Spec
spec = do
    it "should fail with no argument" (checkError [] expectedErrMsg)
    it "should succeed with one argument" (checkSuccess ["hostname"] expectedValue)
    it "should succeed with 4 arguments" (checkSuccess ["hostname", stringDatatype, "unique", "default"] expectedValue)
    it "should succeed with two arguments, the second on being a datatype" (checkSuccess ["hostname", stringDatatype] expectedValue)
    it "should fail when the type mismatched" (checkError ["hostname", boolDatatype] "Datatype mismatched")
    it "should fail with two arguments both strings" (checkError ["hostname", "default"] expectedErrMsg)
