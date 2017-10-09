{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Monad
import System.FilePath.Glob
import Puppet.Parser
import System.Environment
import Puppet.Parser.PrettyPrinter
import Text.PrettyPrint.ANSI.Leijen
import Text.Megaparsec (parse, eof, parseErrorPretty)
import System.Posix.Terminal
import System.Posix.Types
import System.IO
import qualified Data.Text.IO as T

allchecks :: IO ()
allchecks = do
    filelist <- globDir1 (compile "*.pp") "tests/lexer"
    testres <- mapM testparser filelist
    let isgood = all snd testres
    mapM_ (\(rr, t) -> unless t (putStrLn rr)) testres
    unless isgood (error "fail")

-- returns errors
testparser :: FilePath -> IO (String, Bool)
testparser fp =
    fmap (parse (puppetParser <* eof) fp) (T.readFile fp) >>= \case
        Right _ -> return ("PASS", True)
        Left rr -> return (parseErrorPretty rr, False)

check :: String -> IO ()
check fname = do
    putStr fname
    putStr ": "
    res <- fmap (parse puppetParser fname) (T.readFile fname)
    is <- queryTerminal (Fd 1)
    let rfunc = if is
                    then renderPretty 0.2 200
                    else renderCompact
    case res of
        Left rr -> putStrLn (parseErrorPretty rr)
        Right x -> do
            putStrLn ""
            displayIO stdout (rfunc (pretty (ppStatements x)))
            putStrLn ""

main :: IO ()
main = do
    args <- getArgs
    if null args
        then allchecks
        else mapM_ check args
