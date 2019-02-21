{-# LANGUAGE OverloadedLists #-}
module Interpreter.Function.MergeSpec (spec, main) where


import           Helpers

import qualified Data.HashMap.Strict as HM


main :: IO ()
main = hspec spec

spec = do
  spec0
  spec1

evalArgs :: InterpreterMonad PValue -> Either PrettyError (HM.HashMap Text PValue)
evalArgs = dummyEval
        >=> \pv -> case pv of
                      PHash s -> return s
                      _ -> Left ("Expected a string, not " <> PrettyError (pretty pv))

spec0 :: Spec
spec0 = do
  withStdlibFunction "merge" $ \mergeFunc -> do
    let evalArgs' = evalArgs . mergeFunc
    let check args res = case evalArgs' (map PHash args) of
                             Left rr -> expectationFailure (show rr)
                             Right res' -> res' `shouldBe` res
        checkError args ins = case evalArgs' args of
                                  Left rr -> show rr `shouldContain` ins
                                  Right r -> expectationFailure ("Should have errored, received this instead: " <> show r)
    it "should error with invalid arguments" $ do
        checkError [] "Expects at least two hashes"
        checkError [PNumber 1] "Expects at least two hashes"
        checkError [PBoolean True] "Expects at least two hashes"
        checkError ["foo"] "Expects at least two hashes"
    it "should handle empty hashes" $ do
        check [[],[]] []
        check [[],[],[]] []
    it "should merge hashes" $ do
        check [ [("key", "value")], [] ] [("key","value")]
        check [ [], [("key", "value")] ] [("key","value")]
        check [ [("key1", "value1")], [("key2", "value2")], [("key3", "value3")] ] [("key1", "value1"), ("key2", "value2"), ("key3", "value3")]
        check [ [("key", "value1")], [("key", "value2")] ] [("key","value2")]

spec1 :: Spec
spec1 = do
  describe "deep_merge" $ do
  withStdlibFunction "deep_merge" $ \mergeFunc -> do
    let evalArgs' = evalArgs . mergeFunc
    let check args res = case evalArgs' (map PHash args) of
                             Left rr -> expectationFailure (show rr)
                             Right res' -> res' `shouldBe` res
        checkError args ins = case evalArgs' args of
                                  Left rr -> show rr `shouldContain` ins
                                  Right r -> expectationFailure ("Should have errored, received this instead: " <> show r)

    it "should error with invalid arguments" $ do
        checkError [] "Expects at least two hashes"
        checkError [PNumber 1] "Expects at least two hashes"
        checkError [PBoolean True] "Expects at least two hashes"
        checkError ["foo"] "Expects at least two hashes"
    it "should handle empty hashes" $ do
        check [[],[]] []
        check [[],[],[]] []
    it "should deeply merge hashes" $ do
        check [ [("key", "value")], [] ] [("key","value")]
        check [ [], [("key", "value")] ] [("key","value")]
        check [ [("key1", "value1")]
              , [("key2", "value2")]
              , [("key3", "value3")]
              ] [("key1", "value1"), ("key2", "value2"), ("key3", "value3")]
        check [ [("key", "value1")]
              , [("key", "value2")]
              ] [("key","value2")]
        check [ [("key", PHash [("key00", "value00")])]
              , [("key", PHash [("key01", "value01")])]
              ] [("key", PHash [("key01", PString "value01"), ("key00", PString "value00")])]
        check [ [("key", PHash [("key01", "value00")])]
              , [("key", PHash [("key01", "value01")])]
              ] [("key", PHash [("key01", PString "value01")])]
