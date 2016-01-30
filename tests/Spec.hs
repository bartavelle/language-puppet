import Test.Hspec

import qualified InterpreterSpec
import qualified Function.ShellquoteSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Interpreter"  InterpreterSpec.collectorSpec
  describe "The shellquote function"  Function.ShellquoteSpec.spec
