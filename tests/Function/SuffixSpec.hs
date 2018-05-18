{-# LANGUAGE OverloadedLists #-}
module Function.SuffixSpec (spec, main) where

import qualified Data.Text as T

import           Helpers

main :: IO ()
main = hspec spec

fname :: Text
fname = "suffix"

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
      checkSuccess [ PArray ["one"], "post" ] (PArray ["onepost"])
      checkSuccess [ PArray ["one","two","three"], "post" ] (PArray ["onepost","twopost","threepost"])
    it "should work with hashes" $ do
      checkSuccess [(PHash mempty)] (PHash mempty)
      checkSuccess [(PHash mempty), ""] (PHash mempty)
      checkSuccess [(PHash [("one", PNumber 5)] ), "post" ] (PHash [("onepost", PNumber 5)])
      checkSuccess [(PHash [("one", PNumber 5), ("two", "lol"), ("three", PNumber 7)]), "post" ] (PHash [("onepost", PNumber 5), ("twopost", "lol"), ("threepost", PNumber 7)])

