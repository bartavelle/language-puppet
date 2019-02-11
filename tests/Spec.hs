import           Test.Hspec

import           Helpers

import qualified ErbSpec
import qualified HieraSpec
import qualified Interpreter.ClassSpec
import qualified Interpreter.CollectorSpec
import qualified Interpreter.EvalSpec
import qualified Interpreter.EvaluateStatementSpec
import qualified Interpreter.Function.AssertPrivateSpec
import qualified Interpreter.Function.DeleteAtSpec
import qualified Interpreter.Function.EachSpec
import qualified Interpreter.Function.JoinKeysToValuesSpec
import qualified Interpreter.Function.LookupSpec
import qualified Interpreter.Function.MergeSpec
import qualified Interpreter.Function.PrefixSpec
import qualified Interpreter.Function.ShellquoteSpec
import qualified Interpreter.Function.SizeSpec
import qualified Interpreter.Function.SprintfSpec
import qualified Interpreter.Function.SuffixSpec
import qualified Interpreter.Function.WithSpec
import qualified Interpreter.IfSpec
import qualified Parser.DT
import qualified Parser.ExprSpec
import qualified Parser.LexerSpec
import qualified PuppetdbSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Parser" $ do
    describe "Data types" $ do
      Parser.DT.spec
      Parser.ExprSpec.spec
      Parser.LexerSpec.spec
  describe "Interpreter" $ do
    Interpreter.CollectorSpec.spec
    Interpreter.ClassSpec.spec
    Interpreter.EvalSpec.spec
    Interpreter.IfSpec.spec
    Interpreter.EvaluateStatementSpec.spec
    describe "stdlib functions" $ do
      describe "The assert_private function" Interpreter.Function.AssertPrivateSpec.spec
      describe "The join_keys_to_values function" Interpreter.Function.JoinKeysToValuesSpec.spec
      describe "The merge function" Interpreter.Function.MergeSpec.spec
      describe "The size function" Interpreter.Function.SizeSpec.spec
      describe "The delete_at function" Interpreter.Function.DeleteAtSpec.spec
    describe "puppet functions" $ do
      describe "The shellquote function" Interpreter.Function.ShellquoteSpec.spec
      describe "The sprintf function" Interpreter.Function.SprintfSpec.spec
      describe "The each function" Interpreter.Function.EachSpec.spec
      describe "The with function" Interpreter.Function.WithSpec.spec
      describe "The lookup function" Interpreter.Function.LookupSpec.spec
      describe "The suffix function" Interpreter.Function.SuffixSpec.spec
      describe "The prefix function" Interpreter.Function.PrefixSpec.spec
  ErbSpec.spec
  PuppetdbSpec.spec
  HieraSpec.spec
