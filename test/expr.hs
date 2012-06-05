module Main where

import Puppet.DSL.Parser
import Puppet.DSL.Types
import Text.Parsec

testcases = 
    [ ("5 + 3 * 2", PlusOperation (Value $ Integer 5) (MultiplyOperation (Value $ Integer 3) (Value $ Integer 2)) )
    , ("5+2 == 7", EqualOperation ( PlusOperation (Value $ Integer 5) (Value $ Integer 2) ) (Value $ Integer 7) )
    ]

main :: IO ()
main = do
    let testres = map (\(t,v) -> (runParser exprparser () "test" t, v)) testcases
        badstuff = filter (\(Right r,t) -> r /= t) testres
    if (null badstuff)
        then return ()
        else do
            print badstuff
            error "fail"


