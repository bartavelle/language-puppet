module Main where

import System.FilePath.Glob
import Control.Monad.Error
import Puppet.DSL.Loader

main :: IO ()
main = do
    filelist <- globDir [compile "*.pp"] "test/lexer" >>= return . head . fst
    testres <- mapM testparser filelist
    let testsrs = map fst testres
        isgood = and $ map snd testres
        outlist = zip [1..(length testres)] testsrs
    mapM_ (\(n,t) -> putStrLn $ show n ++ " " ++ t) outlist
    if (isgood)
        then return ()
        else error "fail"

-- returns errors
testparser :: FilePath -> IO (String, Bool)
testparser fp = do
    parsed <- runErrorT (parseFile fp)
    case parsed of
        Right _ -> return ("PASS", True)
        Left err -> return (err, False)

