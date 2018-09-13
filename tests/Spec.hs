import           Test.Hspec

import           Helpers

import qualified DT.Parser
import qualified ErbSpec
import qualified EvalSpec
import qualified ExprSpec
import qualified Function.AssertPrivateSpec
import qualified Function.DeleteAtSpec
import qualified Function.EachSpec
import qualified Function.WithSpec
import qualified Function.JoinKeysToValuesSpec
import qualified Function.LookupSpec
import qualified Function.MergeSpec
import qualified Function.ShellquoteSpec
import qualified Function.SizeSpec
import qualified Function.SprintfSpec
import qualified Function.SuffixSpec
import qualified Function.PrefixSpec
import qualified Interpreter.CollectorSpec
import qualified Interpreter.IfSpec
import qualified InterpreterSpec
import qualified LexerSpec
import qualified PuppetdbSpec
import qualified HieraSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Data types" $ do
    describe "Parser" DT.Parser.spec
  EvalSpec.spec
  ExprSpec.spec
  ErbSpec.spec
  LexerSpec.spec
  describe "Interpreter" $ do
    describe "Collector" InterpreterSpec.collectorSpec
    describe "Class include" InterpreterSpec.classIncludeSpec
    describe "Collector (puppet tests)" Interpreter.CollectorSpec.spec
    describe "If" Interpreter.IfSpec.spec
  describe "Puppet functions" $ do
    describe "The shellquote function" Function.ShellquoteSpec.spec
    describe "The sprintf function" Function.SprintfSpec.spec
    describe "The each function" Function.EachSpec.spec
    describe "The with function" Function.WithSpec.spec
    describe "The lookup function" Function.LookupSpec.spec
    describe "The suffix function" Function.SuffixSpec.spec
    describe "The prefix function" Function.PrefixSpec.spec
  describe "stdlib functions" $ do
    describe "The assert_private function" Function.AssertPrivateSpec.spec
    describe "The join_keys_to_values function" Function.JoinKeysToValuesSpec.spec
    describe "The merge function" Function.MergeSpec.spec
    describe "The size function" Function.SizeSpec.spec
    describe "The delete_at function" Function.DeleteAtSpec.spec
  PuppetdbSpec.spec
  HieraSpec.spec
