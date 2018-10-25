{-# LANGUAGE OverloadedLists #-}

module Interpreter.ClassSpec (spec) where

import           Test.Hspec

import           Control.Lens
import qualified Data.Maybe.Strict as S
import qualified Data.Text         as Text

import           Helpers

params:: Parameters
params = [ "param0" :!: S.Nothing :!: S.Nothing ]
topclass = [((TopClass, "foo"), ClassDeclaration $ ClassDecl "foo" params mempty mempty (initialPPos mempty))]


spec = do
  describe "Class" $ do
    includeSpec
  describe "Params" $ do
    paramsSpec

includeSpec = do
  describe "Multiple loading" $ do
    it "should work when using several include statements" $
      pureCatalog' topclass (Text.unlines ["include foo", "include foo"]) `shouldSatisfy` (has _Right)
    it "should work when using class before include" $
      pureCatalog' topclass (Text.unlines [ "class { 'foo': }", "include foo"]) `shouldSatisfy` (has _Right)
    it "should fail when using include before class" $
      pureCatalog' topclass (Text.unlines [ "include foo", "class { 'foo': }" ]) `shouldSatisfy` (has _Left)

paramsSpec = do
  describe "Parameters" $ do
    it "should fail when instantiate with an unknown params" $
      pureCatalog' topclass (Text.unlines ["class {'foo': param1 => 1 }"]) `shouldSatisfy` (has _Left)
    it "should succeed when instantiate with a correct param" $
      pureCatalog' topclass (Text.unlines ["class {'foo': param0 => 1 }"]) `shouldSatisfy` (has _Right)
    it "should fail when instantiate with a missing param" $ do
      pending
      pureCatalog' topclass (Text.unlines ["class {'foo': }"]) `shouldSatisfy` (has _Left)
