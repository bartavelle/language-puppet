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
    [ ("5 + 3 * 2", Addition (PValue $ UString "5") (Multiplication (PValue $ UString "3") (PValue $ UString "2")) )
    , ("5+2 == 7", Equal ( Addition (PValue $ UString "5") (PValue $ UString "2") ) (PValue $ UString "7") )
    , ("include foo::bar",  PValue (UFunctionCall "include" (V.fromList [PValue (UString "foo::bar")])) )
    ]

main :: IO ()
main = do
    testres <- forM testcases $ \(a,b) -> do
        na <- runMyParser (expression <* eof) "tests" a
        return (na, b)
    let isFailure (Left x, _) = Just (show x)
        isFailure (Right x, e) = if x == e
                                     then Nothing
                                     else Just (displayS (renderPretty 0.4 80 (pretty x)) "" ++ "\n" ++ displayS (renderPretty 0.4 80 (pretty e)) "")
        bads = mapMaybe isFailure testres
    unless (null bads) $ do
        mapM_ putStrLn bads
        error "failed"
