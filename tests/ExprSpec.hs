{-# LANGUAGE OverloadedLists #-}
module ExprSpec (spec) where

import           Test.Hspec
import           Test.Hspec.Megaparsec
import           Text.Megaparsec

import           Puppet.Parser
import           Puppet.Parser.Internal
import           XPrelude

expressions :: [(Text, Expression)]
expressions =
    [ ("5 + 3 * 2", 5 + 3 * 2)
    , ("5+2 == 7", Equal (5 + 2) 7)
    , ("include(foo::bar)",  Terminal (UFunctionCall "include" ["foo::bar"] ))
    , ("$y ? {\
     \ undef   => 'undef',\
     \ default => 'default',\
    \ }",  ConditionalValue (Terminal (UVariableReference "y"))
           [ SelectorValue UUndef :!: Terminal (UString "undef")
           , SelectorDefault :!: Terminal (UString "default")])
    , ("$x", Terminal (UVariableReference "x"))
    , ("x($y)", Terminal (UFunctionCall "x" [ Terminal (UVariableReference "y") ]))
    , ("\"${x}\"", Terminal (UInterpolable [Terminal (UVariableReference "x")]))
    , ("\"${x[3]}\"", Terminal (UInterpolable [Lookup (Terminal (UVariableReference "x")) 3]))
    , ("\"${x[$y]}\"", Terminal (UInterpolable [Lookup (Terminal (UVariableReference "x")) (Terminal (UVariableReference "y")) ]))
    , ("\"${x($y)}\"", Terminal (UInterpolable [ Terminal (UFunctionCall "x" [ Terminal (UVariableReference "y") ] ) ] ))
    , ("\"${x($y)}$'\"", Terminal (UInterpolable [ Terminal (UFunctionCall "x" [ Terminal (UVariableReference "y") ])
                                                 , Terminal (UString "$"),Terminal (UString "'")]))
    ]


testExpression (t,e) = it ("should parse " ++ show t) $ parse (expression <* eof) "" t `shouldParse` e

spec = do
  describe "Expression parser" $ mapM_ testExpression expressions
