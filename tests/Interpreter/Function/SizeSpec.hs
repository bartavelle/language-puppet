{-# LANGUAGE OverloadedLists #-}

module Interpreter.Function.SizeSpec (spec, main) where

import Helpers

main :: IO ()
main = hspec spec

evalArgs :: InterpreterMonad PValue -> Either PrettyError Scientific
evalArgs =
  dummyEval
    >=> \pv -> case pv of
      PNumber s -> return s
      _ -> Left ("Expected a string, not " <> PrettyError (pretty pv))

spec :: Spec
spec = withStdlibFunction "size" $ \sizeFunc -> do
  let evalArgs' = evalArgs . sizeFunc
  let check args res = case evalArgs' args of
        Left rr -> expectationFailure (show rr)
        Right res' -> res' `shouldBe` res
      checkError args ins = case evalArgs' args of
        Left rr -> show rr `shouldContain` ins
        Right r -> expectationFailure ("Should have errored, received this instead: " <> show r)
  it "should error with no arguments" (checkError [] "a single argument")
  it "should error with numerical arguments" (checkError [PNumber 1] "size(): Expects ")
  it "should error with boolean arguments" (checkError [PBoolean True] "size(): Expects ")
  -- Not conformant:
  -- it "should error with numerical arguments" (checkError ["1"] "size(): Expects ")
  it "should handle arrays" $ do
    check [PArray []] 0
    check [PArray ["a"]] 1
    check [PArray ["one", "two", "three"]] 3
    check [PArray ["one", "two", "three", "four"]] 4
  it "should handle hashes" $ do
    check [PHash []] 0
    check [PHash [("1", "2")]] 1
    check [PHash [("1", "2"), ("3", "4")]] 2
  it "should handle strings" $ do
    check [""] 0
    check ["a"] 1
    check ["ab"] 2
    check ["abcd"] 4
