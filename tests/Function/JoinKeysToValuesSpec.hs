{-# LANGUAGE OverloadedLists #-}
module Function.JoinKeysToValuesSpec (spec) where

import           Test.Hspec

import qualified Data.Foldable as F
import           Data.Monoid

import           Puppet.Interpreter

import           Helpers

spec :: Spec
spec = withStdlibFunction "join_keys_to_values" $ \jkvFunc -> it "Should work as expected" $ do
    let eval h s = case dummyEval (jkvFunc [PHash h, PString s]) of
                      Left rr -> Left (renderToString (getError rr))
                      Right (PArray vals) -> Right (F.toList vals)
                      Right v -> Left ("Expected an array, not: " <> renderToString v)
    eval [] "" `shouldBe` Right []
    eval [] ":" `shouldBe` Right []
    eval [("key","value")] "" `shouldBe` Right ["keyvalue"]
    eval [("key","value")] ":" `shouldBe` Right ["key:value"]
    eval [("key",PUndef)] ":" `shouldBe` Right ["key:"]
    case eval [("key1","value1"),("key2","value2")] ":" of
        Left rr -> fail rr
        Right lst -> lst `shouldMatchList` ["key1:value1", "key2:value2"]
