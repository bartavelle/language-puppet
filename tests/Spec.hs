import Test.Hspec

import qualified InterpreterSpec
import qualified Function.ShellquoteSpec
import qualified Function.SizeSpec
import qualified Function.MergeSpec
import qualified Function.EachSpec
import qualified Function.AssertPrivateSpec
import qualified Function.JoinKeysToValuesSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Interpreter" $ do
    describe "Collector" InterpreterSpec.collectorSpec
    describe "Class include" InterpreterSpec.classIncludeSpec
  describe "The shellquote function" Function.ShellquoteSpec.spec
  describe "stdlib functions" $ do
      describe "The assert_private function" Function.AssertPrivateSpec.spec
      describe "The each function" Function.EachSpec.spec
      describe "The join_keys_to_values function" Function.JoinKeysToValuesSpec.spec
      describe "The merge function" Function.MergeSpec.spec
      describe "The size function" Function.SizeSpec.spec
