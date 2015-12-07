import Test.Hspec

import qualified InterpreterSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Interpreter"  InterpreterSpec.spec
