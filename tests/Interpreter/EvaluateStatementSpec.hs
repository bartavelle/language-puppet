{-# LANGUAGE OverloadedLists #-}

module Interpreter.EvaluateStatementSpec where

import qualified Data.Text as Text
import Helpers

main :: IO ()
main = hspec spec

shouldNotify :: [Text] -> PValue -> Expectation
shouldNotify s expected = do
  catalog <- case pureCatalog (Text.unlines s) of
    Left rr -> fail rr
    Right (x, _) -> pure x
  let msg = catalog ^? at (RIdentifier "notify" "test") . _Just . rattributes . ix "message"
  msg `shouldBe` Just expected

spec :: Spec
spec = do
  describe "evaluate statement" $ do
    it "should evaluate simple variable assignment" $
      ["$a = 0", "notify { 'test': message => \"a is ${a}\"}"] `shouldNotify` "a is 0"
    it "should evaluate chained variables assignment" $
      ["$a = $b = 0", "notify { 'test': message => \"b is ${b}\"}"] `shouldNotify` "b is 0"
