module Main where

import qualified Data.Text as T
import Puppet.PP
import Puppet.Parser
import Puppet.Interpreter.Pure
import Puppet.Interpreter.Types
import Puppet.Interpreter.Resolve

import Control.Applicative
import Text.Parser.Combinators (eof)
import Data.Either (lefts)

pureTests :: [T.Text]
pureTests = [ "4 + 2 == 6"
            , "[1,2][1] == 2"
            , "[1,[1,2]][1][0] == 1"
            , "[1,2,3] + [4,5,6] == [1,2,3,4,5,6]"
            , "{a => 1} + {b => 2} == {a=>1, b=>2 }"
            , "[1,2,3] << 10 == [1,2,3,10]"
            , "[1,2,3] << [4,5] == [1,2,3,[4,5]]"
            , "4 / 2.0 == 2"
            ]

main :: IO ()
main = do
    let check :: T.Text -> Either String ()
        check t = case runPParser (expression <* eof) "dummy" t of
                      Left rr -> Left (T.unpack t ++ " -> " ++ show rr)
                      Right e -> case dummyEval (resolveExpression e) of
                                     Right (PBoolean True) -> Right ()
                                     Right x -> Left (T.unpack t ++ " -> " ++ show (pretty x))
                                     Left rr -> Left (T.unpack t ++ " -> " ++ show rr)
    mapM_ putStrLn (lefts $ map check pureTests)
