import Test.Hspec

import qualified InterpreterSpec
import qualified Function.ShellquoteSpec
import qualified Function.SizeSpec
import qualified Function.MergeSpec
import qualified Function.EachSpec
import qualified Function.AssertPrivateSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Interpreter" $ do
    describe "Collector" InterpreterSpec.collectorSpec
    describe "Class include" InterpreterSpec.classIncludeSpec
  describe "The shellquote function" Function.ShellquoteSpec.spec
  describe "stdlib functions" $ do
      describe "The size function" Function.SizeSpec.spec
      describe "The merge function" Function.MergeSpec.spec
      describe "The each function" Function.EachSpec.spec
      describe "The assert_private function" Function.AssertPrivateSpec.spec
