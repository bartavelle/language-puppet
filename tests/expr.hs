module Main where

import           Control.Applicative
import           Control.Arrow               (first)
import           Control.Monad
import           Data.Maybe
import qualified Data.Text                   as T
import           Data.Tuple.Strict
import qualified Data.Vector                 as V
import           Puppet.Parser
import           Puppet.Parser.PrettyPrinter ()
import           Puppet.Parser.Types
import           Text.Parser.Combinators
import           Prelude

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
    , ("\"${x}\"", Terminal (UInterpolable (V.fromList [Terminal (UVariableReference "x")])))
    , ("\"${x[3]}\"", Terminal (UInterpolable (V.fromList [Lookup (Terminal (UVariableReference "x")) 3])))
    , ("\"${x[$y]}\"", Terminal (UInterpolable (V.fromList [Lookup (Terminal (UVariableReference "x")) (Terminal (UVariableReference "y")) ])))
    ]

main :: IO ()
main = do
    let testres = map (first (runPParser (expression <* eof) "tests")) testcases
        isFailure (Left x, _) = Just (show x)
        isFailure (Right x, e) = if x == e
                                     then Nothing
                                     else Just ("Expected: " ++ show e ++ "\nActual: " ++ show x)
        bads = mapMaybe isFailure testres
    unless (null bads) $ do
        mapM_ putStrLn bads
        error "failed"
