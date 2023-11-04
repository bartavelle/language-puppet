module Interpreter.Function.LookupSpec (spec, main) where

import Helpers

main :: IO ()
main = hspec spec

fname :: Text
fname = "lookup"

expectedErrMsg :: String
expectedErrMsg = "Wrong set of arguments"

-- See dummyFacts defined in Pure.hs
testkey :: Expression
testkey = "foo"

expectedValue :: Text
expectedValue = "dummy"

checkSuccess :: [Expression] -> Text -> Expectation
checkSuccess = checkExprsSuccess fname

checkError :: [Expression] -> String -> Expectation
checkError = checkExprsError fname

boolDatatype, stringDatatype :: Expression
boolDatatype = Terminal (UDataType UDTBoolean)
stringDatatype = Terminal (UDataType (UDTString Nothing Nothing))

spec :: Spec
spec = do
  it "should fail with no argument" (checkError [] expectedErrMsg)
  it "should succeed with one argument" (checkSuccess [testkey] expectedValue)
  it "should succeed with 4 arguments" (checkSuccess [testkey, stringDatatype, "unique", "default"] expectedValue)
  it "should fail with an unknown merge strategy" (checkError [testkey, stringDatatype, "joe", "default"] "Unknown merge strategy")
  it "should succeed with two arguments, the second one being a datatype" (checkSuccess [testkey, stringDatatype] expectedValue)
  it "should fail when the type mismatched" (checkError [testkey, boolDatatype] "Datatype mismatched")
  it "should fail with two arguments both strings" (checkError [testkey, "default"] expectedErrMsg)
