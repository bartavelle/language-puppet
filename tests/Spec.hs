import Test.Hspec

import qualified InterpreterSpec
import qualified Function.ShellquoteSpec
import qualified Function.SizeSpec
import qualified Function.MergeSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Interpreter"  InterpreterSpec.collectorSpec
  describe "The shellquote function"  Function.ShellquoteSpec.spec
  describe "stdlib functions" $ do
      describe "The size function"  Function.SizeSpec.spec
      describe "The merge function"  Function.MergeSpec.spec
