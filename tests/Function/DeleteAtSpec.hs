{-# LANGUAGE OverloadedLists #-}
module Function.DeleteAtSpec (spec, main) where

import           Test.Hspec

import           Data.Monoid

import           Puppet.Interpreter.Pure
import           Puppet.Interpreter.Types

import           Helpers

main :: IO ()
main = hspec spec

spec :: Spec
spec = withStdlibFunction "delete_at" $ \deleteAtFunc -> do
    let evalArgs' = dummyEval . deleteAtFunc
        narray = PArray . fmap PNumber
        check a b res = case evalArgs' [narray a, PNumber b] of
                             Left rr -> expectationFailure (show rr)
                             Right res' -> res' `shouldBe` narray res
        checkError args ins = case evalArgs' args of
                                  Left rr -> show rr `shouldContain` ins
                                  Right r -> expectationFailure ("Should have errored, received this instead: " <> show r)
    it "should error with invalid arguments" $ do
        checkError [] "expects 2 arguments"
        checkError [PNumber 1] "expects 2 arguments"
        checkError ["foo", "bar"] "expects its first argument to be an array"
        checkError [ narray [0,1,2], PNumber 3 ] "Out of bounds access"
    it "should work otherwise" $ do
        check [0,1,2] 1 [0,2]
    it "should work for negative positions" $ do
        pending
        check [0,1,2] (-1) [0,1]
        check [0,1,2] (-4) [0,1,2]
