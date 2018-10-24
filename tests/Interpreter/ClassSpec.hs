module Interpreter.ClassSpec (spec) where

import           Test.Hspec

import           Control.Lens
import qualified Data.Text as Text

import           Helpers

spec = do
  describe "Class" $ do
    includeSpec
  describe "Params" $ do
    includeSpec
includeSpec = do
  describe "Multiple loading" $ do
    it "should work when using several include statements" $
      pureCatalog (Text.unlines ["include foo", "include foo"]) `shouldSatisfy` (has _Right)
    it "should work when using class before include" $
      pureCatalog (Text.unlines [ "class { 'foo': }", "include foo"]) `shouldSatisfy` (has _Right)
    it "should fail when using include before class" $
      pureCatalog (Text.unlines [ "include foo", "class { 'foo': }" ]) `shouldSatisfy` (has _Left)
