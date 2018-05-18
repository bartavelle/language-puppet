{-# LANGUAGE OverloadedLists #-}
module Function.PrefixSpec (spec, main) where

import qualified Data.Text as T

import           Helpers

main :: IO ()
main = hspec spec

fname :: Text
fname = "prefix"

spec :: Spec
spec = withStdlibFunction fname $ \tester -> do
    let checkError input expectederror =
           case dummyEval (tester input) of
             Left rr -> show (getError rr) `shouldStartWith` (T.unpack fname ++ ": " ++ expectederror)
             Right _ -> expectationFailure "should have failed"
        checkSuccess input expected =
           case dummyEval (tester input) of
             Left rr -> expectationFailure (show rr)
             Right r -> r `shouldBe` expected
    it "should fail with no argument" (checkError [] "expects two arguments")
    it "should fail if the first argument isn't an array or hash" (checkError ["lol"] "expects the first argument to be an array or a hash")
    it "should fail if the second argument isn't a string" $ do
      checkError [PArray [], PNumber 1] "expects the second argument to be a string"
      checkError [PArray [], PArray []] "expects the second argument to be a string"
    it "should work with arrays" $ do
      checkSuccess [ PArray []] (PArray [])
      checkSuccess [ PArray [], ""] (PArray [])
      checkSuccess [ PArray ["one"], "pre" ] (PArray ["preone"])
      checkSuccess [ PArray ["one","two","three"], "pre" ] (PArray ["preone","pretwo","prethree"])
    it "should work with hashes" $ do
      checkSuccess [(PHash mempty)] (PHash mempty)
      checkSuccess [(PHash mempty), ""] (PHash mempty)
      checkSuccess [(PHash [("one", PNumber 5)] ), "pre" ] (PHash [("preone", PNumber 5)])
      checkSuccess [(PHash [("one", PNumber 5), ("two", "lol"), ("three", PNumber 7)]), "pre" ] (PHash [("preone", PNumber 5), ("pretwo", "lol"), ("prethree", PNumber 7)])

