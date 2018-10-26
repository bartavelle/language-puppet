{-# LANGUAGE OverloadedLists #-}
-- | Directly ported from puppet specs
module Interpreter.IfSpec (spec) where

import           Test.Hspec

import           Control.Lens
import qualified Data.Text as Text

import           Helpers

shouldFail :: [Text] -> Expectation
shouldFail content = let cat = pureCatalog (Text.unlines content)
                     in  cat `shouldSatisfy` has _Left

shouldNotFail :: [Text] -> Expectation
shouldNotFail content = let cat = pureCatalog (Text.unlines content)
                        in  cat `shouldSatisfy` has _Right

spec :: Spec
spec = do
  describe "If" $ do
    it "doesn't enter false conditions" $ shouldNotFail
        [ "if (false) { fail ':(' }" ]
    it "enters true conditions" $ shouldFail
        [ "if (true) { fail ':(' }" ]
    it "enters empty string conditions" $ shouldFail
        [ "if '' { fail ':(' }" ]
    it "not (unknown variable) is true" $ shouldFail
        [ "if (!$::unknown123) { fail ':(' }" ]
