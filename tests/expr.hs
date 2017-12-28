module Main where

import qualified Data.Text                   as T
import           Data.Tuple.Strict
import qualified Data.Vector                 as V
import           Puppet.Parser
import           Text.Megaparsec

import           Test.Hspec
import           Test.Hspec.Megaparsec

testcases :: [(T.Text, Expression)]
testcases =
    [ ("5 + 3 * 2", 5 + 3 * 2)
    , ("5+2 == 7", Equal (5 + 2) 7)
    , ("include(foo::bar)",  Terminal (UFunctionCall "include" (V.singleton "foo::bar") ))
    , ("$y ? {\
     \ undef   => 'undef',\
     \ default => 'default',\
    \ }",  ConditionalValue (Terminal (UVariableReference "y"))
           (V.fromList [SelectorValue UUndef :!: Terminal (UString "undef")
                       ,SelectorDefault :!: Terminal (UString "default")]))
    , ("$x", Terminal (UVariableReference "x"))
    , ("x($y)", Terminal (UFunctionCall "x" (V.singleton (Terminal (UVariableReference "y")))))
    , ("\"${x}\"", Terminal (UInterpolable (V.fromList [Terminal (UVariableReference "x")])))
    , ("\"${x[3]}\"", Terminal (UInterpolable (V.fromList [Lookup (Terminal (UVariableReference "x")) 3])))
    , ("\"${x[$y]}\"", Terminal (UInterpolable (V.fromList [Lookup (Terminal (UVariableReference "x")) (Terminal (UVariableReference "y")) ])))
    , ("\"${x($y)}\"", Terminal (UInterpolable (V.fromList [ Terminal (UFunctionCall "x" (V.singleton (Terminal (UVariableReference "y")))) ])))
    , ("\"${x($y)}$'\"", Terminal (UInterpolable (V.fromList [ Terminal (UFunctionCall "x" (V.singleton (Terminal (UVariableReference "y")))),Terminal (UString "$"),Terminal (UString "'")])))
    ]

main :: IO ()
main = hspec $ describe "Expression parser" $ mapM_ test testcases
    where
        test (t,e) = it ("should parse " ++ show t) $ parse (expression <* eof) "" t `shouldParse` e
