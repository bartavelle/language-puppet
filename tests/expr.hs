module Main where

import Text.PrettyPrint.ANSI.Leijen

import Puppet.Parser
import Puppet.Parser.PrettyPrinter()
import Puppet.Parser.Types
import Text.Parser.Combinators
import Control.Monad
import Data.Maybe
import qualified Data.Text as T
import Control.Applicative
import qualified Data.Vector as V

testcases :: [(T.Text, Expression)]
testcases =
    [ ("5 + 3 * 2", 5 + 3 * 2)
    , ("5+2 == 7", Equal (5 + 2) 7)
    , ("include(foo::bar)",  Terminal (UFunctionCall "include" (V.singleton "foo::bar") ))
    ]

main :: IO ()
main = do
    let testres = map (\(a,b) -> (runPParser (expression <* eof) "tests" a, b)) testcases
        isFailure (Left x, _) = Just (show x)
        isFailure (Right x, e) = if x == e
                                     then Nothing
                                     else Just (displayS (renderPretty 0.4 80 (pretty x)) "" ++ "\n" ++ displayS (renderPretty 0.4 80 (pretty e)) "")
        bads = mapMaybe isFailure testres
    unless (null bads) $ do
        mapM_ putStrLn bads
        error "failed"
