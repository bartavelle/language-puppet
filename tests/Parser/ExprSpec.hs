{-# LANGUAGE OverloadedLists #-}

module Parser.ExprSpec (spec) where

import Puppet.Parser
import Puppet.Parser.Internal
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec
import XPrelude

expressions :: [(Text, Expression)]
expressions =
  [ ("5 + 3 * 2", 5 + 3 * 2),
    ("5+2 == 7", Equal (5 + 2) 7),
    ("include(foo::bar)", Terminal (UFunctionCall "include" ["foo::bar"])),
    ("fail(('foo'))", Terminal (UFunctionCall "fail" ["foo"])),
    ("test(foo,bar)", Terminal (UFunctionCall "test" ["foo", "bar"])),
    ("extlib::test()", Terminal (UFunctionCall "extlib::test" [])),
    ("extlib::test(fail('foo'))", Terminal (UFunctionCall "extlib::test" [Terminal (UFunctionCall "fail" [Terminal (UString "foo")])])),
    ("test(extlib::test())", Terminal (UFunctionCall "test" [Terminal (UFunctionCall "extlib::test" [])])),
    ("test ( foo , bar )", Terminal (UFunctionCall "test" ["foo", "bar"])),
    ( "$y ? {\
      \ undef   => 'undef',\
      \ default => 'default',\
      \ }",
      ConditionalValue
        (Terminal (UVariableReference "y"))
        [ SelectorValue UUndef :!: Terminal (UString "undef"),
          SelectorDefault :!: Terminal (UString "default")
        ]
    ),
    ("$x", Terminal (UVariableReference "x")),
    ("x($y)", Terminal (UFunctionCall "x" [Terminal (UVariableReference "y")])),
    ("\"$\"", Terminal (UInterpolable [Terminal (UString "$")])),
    ("\"${x}\"", Terminal (UInterpolable [Terminal (UVariableReference "x")])),
    ("$x[ 3 ]", Lookup (Terminal (UVariableReference "x")) (Terminal (UNumber 3))),
    ("\"${ os[ 'architecture' ]}\"", Terminal (UInterpolable [Lookup (Terminal (UVariableReference "os")) (Terminal (UString "architecture"))])),
    ("\"${facts['os']['architecture']}\"", Terminal (UInterpolable [Lookup (Lookup (Terminal (UVariableReference "facts")) (Terminal (UString "os"))) (Terminal (UString "architecture"))])),
    ("\"${x[$y]}\"", Terminal (UInterpolable [Lookup (Terminal (UVariableReference "x")) (Terminal (UVariableReference "y"))])),
    ("\"${x($y)}\"", Terminal (UInterpolable [Terminal (UFunctionCall "x" [Terminal (UVariableReference "y")])])),
    ( "\"${x($y)}$'\"",
      Terminal
        ( UInterpolable
            [ Terminal (UFunctionCall "x" [Terminal (UVariableReference "y")]),
              Terminal (UString "$"),
              Terminal (UString "'")
            ]
        )
    )
  ]

invalid :: [Text]
invalid =
  [ "$os['name]",
    -- pending
    -- , "$os ['name']"
    -- interpolation
    "\"${os['name]}\"",
    "\"${os[name}\"",
    "\"${os[name]\""
  ]

testExpression (t, e) = it ("should parse " <> toS t) $ parse (expression <* eof) "" t `shouldParse` e

testInvalid s = it ("rejects " <> toS s) $ shouldFailOn (parse (expression <* eof) "") s

spec = do
  describe "Expression parser" $ mapM_ testExpression expressions
  describe "Invalid expression" $ mapM_ testInvalid invalid
