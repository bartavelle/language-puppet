{-# LANGUAGE OverloadedLists #-}

module Interpreter.ClassSpec (spec) where

import           Test.Hspec

import           Control.Lens
import qualified Data.Text         as Text

import           Helpers

spec = do
  describe "Class" $ do
    it "should work when using several include statements" $
      pureCatalog (Text.unlines ["class foo {}", "include foo", "include foo"]) `shouldSatisfy` (has _Right)
    it "should work when using class before include" $
      pureCatalog (Text.unlines ["class foo {}", "class { 'foo': }", "include foo"]) `shouldSatisfy` (has _Right)
    it "should fail when using include before class" $
      pureCatalog (Text.unlines ["class foo {}", "include foo", "class { 'foo': }" ]) `shouldSatisfy` (has _Left)
    it "should fail if the class is not defined" $
      pureCatalog (Text.unlines ["include foo"]) `shouldSatisfy` (has _Left)
  describe "Parameters" $ do
    it "should fail when declaring a class with an unknown params" $
      pureCatalog (Text.unlines ["class foo ($param0){}", "class {'foo': param1 => 1 }"]) `shouldSatisfy` (has _Left)
    it "should succeed when declaring a class with a correct param" $
      pureCatalog (Text.unlines ["class foo ($param0){}", "class {'foo': param0 => 1 }"]) `shouldSatisfy` (has _Right)
    it "should fail when the type of the attribute is wrong"  $ do
      pureCatalog (Text.unlines ["class foo (String $param0){}", "class {'foo': param0 => 1 }"]) `shouldSatisfy` (has _Left)
    it "should fail when declaring with a missing param" $ do
      pureCatalog (Text.unlines ["class foo ($param0){}", "class {'foo': }"]) `shouldSatisfy` (has _Left)
    it "should succeed with missing param and an optional type" $ do
      pureCatalog (Text.unlines ["class foo (Optional[String] $param0){}", "class {'foo': }"]) `shouldSatisfy` (has _Right)
