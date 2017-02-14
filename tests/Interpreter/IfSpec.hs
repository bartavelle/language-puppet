{-# LANGUAGE OverloadedLists #-}
-- | Directly ported from puppet specs
module Interpreter.IfSpec (spec) where

import           Test.Hspec

import           Control.Lens
import           Control.Monad.Except
import           Data.Text (Text)
import qualified Data.Text as T

import           Helpers
import           Puppet.Interpreter.Types

{-
shouldReturn :: [Text] -> [PValue] -> Expectation
shouldReturn content expectedMessages = do
    cat <- case runExcept (getCatalog (T.unlines content)) of
               Left rr -> fail rr
               Right x -> return x
    _foo cat
-}

shouldFail :: [Text] -> Expectation
shouldFail content = let cat :: Either String FinalCatalog
                         cat = runExcept (getCatalog (T.unlines content))
                     in  cat `shouldSatisfy` has _Left

shouldNotFail :: [Text] -> Expectation
shouldNotFail content = let cat :: Either String FinalCatalog
                            cat = runExcept (getCatalog (T.unlines content))
                        in  cat `shouldSatisfy` has _Right

spec :: Spec
spec = do
    it "doesn't enter false conditions" $ shouldNotFail
        [ "if (false) { fail ':(' }" ]
    it "enters true conditions" $ shouldFail
        [ "if (true) { fail ':(' }" ]
    it "not (unknown variable) is true" $ shouldFail
        [ "if (!$::unknown123) { fail ':(' }" ]
