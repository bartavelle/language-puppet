{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import           XPrelude

import qualified Data.Text          as T

import           Data.Foldable      (forM_)
import           Test.Hspec
import           Text.Megaparsec    (eof, parse)

import           Puppet.Interpreter
import           Puppet.Runner
import           Puppet.Parser

pureTests :: [T.Text]
pureTests = [ "4 + 2 == 6"
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

main :: IO ()
main = do
  let check :: T.Text -> Either Text ()
      check t = case parse (expression <* eof) "dummy" t of
                  Left rr -> Left (t <> " -> " <> show rr)
                  Right e -> case dummyEval (resolveExpression e) of
                    Right (PBoolean True) -> Right ()
                    Right x -> Left (t <> " -> " <> show (pretty x))
                    Left rr -> Left (t <> " -> " <> show rr)
      runcheck :: String -> IO ()
      runcheck t = case parse (expression <* eof) "dummy" (T.pack t) of
        Left rr -> panic ("Can't parse: " <> show rr)
        Right e -> case dummyEval (resolveExpression e) of
          Right x -> print (pretty x)
          Left rr -> panic ("Can't eval: " <> show rr)
  args <- getArgs
  if null args
    then hspec $ describe "evaluation" $ forM_ pureTests $ \t -> it ("should evaluate " ++ show t) $ either panic (const True) (check t)
    else mapM_ runcheck args
