module Main where

import qualified Data.Text as T
import Puppet.PP
import Puppet.Parser
import Puppet.Interpreter.Pure
import Puppet.Interpreter.Types
import Puppet.Interpreter.Resolve

import System.Environment
import Test.Hspec
import Text.Megaparsec (eof)
import Data.Foldable (forM_)

pureTests :: [T.Text]
pureTests = [ "4 + 2 == 6"
            , "[1,2][1] == 2"
            , "[1,[1,2]][1][0] == 1"
            , "[1,2,3] + [4,5,6] == [1,2,3,4,5,6]"
            , "{a => 1} + {b => 2} == {a=>1, b=>2 }"
            , "[1,2,3] << 10 == [1,2,3,10]"
            , "[1,2,3] << [4,5] == [1,2,3,[4,5]]"
            , "4 / 2.0 == 2"
            , "$settings::confdir == '/etc/puppet'"
            , "regsubst('127', '([0-9]+)', '<\\1>', 'G') == '<127>'"
            , "regsubst(['1','2','3'], '([0-9]+)', '<\\1>', 'G') == ['<1>','<2>','<3>']"
            , "versioncmp('2.1','2.2') == -1"
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
        runcheck :: String -> IO ()
        runcheck t = case runPParser (expression <* eof) "dummy" (T.pack t) of
                         Left rr -> error ("Can't parse: " ++ show rr)
                         Right e -> case dummyEval (resolveExpression e) of
                                        Right x -> print (pretty x)
                                        Left rr -> error ("Can't eval: " ++ show rr)
    args <- getArgs
    if null args
        then hspec $ describe "evaluation" $ forM_ pureTests $ \t -> it ("should evaluate " ++ show t) $ either error (const True) (check t)
        else mapM_ runcheck args
