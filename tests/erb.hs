module Main where

import System.Environment
import Erb.Parser
import Erb.Ruby
import Control.Monad (when)

checkParse :: FilePath -> IO (Maybe [RubyStatement])
checkParse fp = parseErbFile fp >>= \c ->
    case c of
        Left rr -> print rr >> return Nothing
        Right x -> return (Just x)

main :: IO ()
main = do
    a <- getArgs
    r <- mapM checkParse a
    putStrLn (show (length $ filter (/= Nothing) r) ++ "/" ++ show (length a) ++ " files parsed")
    when (length a == 1) (mapM_  print r)
