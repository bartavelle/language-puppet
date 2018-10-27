module Interpreter.EvalSpec (spec) where


import           Test.Hspec
import           Text.Megaparsec       (eof, parse)

import           Puppet.Interpreter
import           Puppet.Parser.Internal
import           Puppet.Runner
import           XPrelude

evaluations = [ "4 + 2 == 6"
            , "[1,2][1] == 2"
            , "[1,[1,2]][1][0] == 1"
            , "[1,2,3] + [4,5,6] == [1,2,3,4,5,6]"
            , "{a => 1} + {b => 2} == {a=>1, b=>2 }"
            , "[1,2,3] << 10 == [1,2,3,10]"
            , "[1,2,3] << [4,5] == [1,2,3,[4,5]]"
            , "4 / 2.0 == 2"
            , "$architecture == 'amd64'"
            , "$facts['architecture'] == 'amd64'"
            , "$settings::confdir == '/etc/puppet'"
            , "regsubst('127', '([0-9]+)', '<\\1>', 'G') == '<127>'"
            , "regsubst(['1','2','3'], '([0-9]+)', '<\\1>', 'G') == ['<1>','<2>','<3>']"
            , "versioncmp('2.1','2.2') == -1"
            , "inline_template('a','b') == 'ab'"
            ]

testEvaluation t =
  let item = it ("should evaluate " <> t) in
  case check (toS t) of
    Left ctx -> context ctx $ item False
    Right b  -> item b

check :: Text -> Either String Bool
check t =
  case parse (expression <* eof) "dummy" t of
    Left _ -> Left $ "Parsing error: are you sure the evaluation is correct ?"
    Right e -> case dummyEval (resolveExpression e) of
      Right (PBoolean True) -> Right True
      _                     -> Right False

spec = do
  describe "Evaluation of expressions" $ mapM_ testEvaluation evaluations
