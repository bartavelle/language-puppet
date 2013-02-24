module Main where

import System.FilePath.Glob
import Control.Monad.Error
import Puppet.DSL.Loader
import Puppet.DSL.Types
import Puppet.Interpreter.Catalog
import Puppet.Interpreter.Types
import Puppet.NativeTypes
import qualified Data.Map as Map
import Data.Either
import Data.List
import Text.Parsec.Pos
import qualified Data.Text as T

getstatement :: Map.Map (TopLevelType, T.Text) Statement -> TopLevelType -> T.Text -> IO (Either String Statement)
getstatement stmtlist toplevel name = case (Map.lookup (toplevel, name) stmtlist) of
    Just x -> return $ Right x
    Nothing -> return $ Left "not found"

gettemplate :: T.Text -> T.Text -> c -> IO (Either String T.Text)
gettemplate n _ _ = return $ Right n

main :: IO ()
main = do
    filelist <- globDir [compile "*.pp"] "test/interpreter" >>= return . head . fst
    testres <- mapM testinterpreter filelist
    let testsrs = map fst testres
        isgood = and $ map snd testres
        outlist = zip [1..(length testres)] testsrs
    mapM_ (\(n,t) -> putStrLn $ show n ++ " " ++ t) outlist
    if (isgood)
        then return ()
        else error "fail"

pdb :: b -> c -> IO (Either String a)
pdb _ _ = return $ Left "No puppetdb"

testinterpreter :: FilePath -> IO (String, Bool)
testinterpreter fp = do
    parsed <- runErrorT (parseFile fp)
    case parsed of
        Left err -> return (err, False)
        Right p -> do
            let facts = Map.fromList [("::hostname",ResolvedString "test"), ("::fqdn", ResolvedString "fqdn")]
                toplevels = map convertTopLevel p
                oktoplevels = rights toplevels
                othertoplevels = lefts toplevels
                topclass = ClassDeclaration "::" Nothing [] othertoplevels (initialPos fp)
                stmtpmap :: Map.Map (TopLevelType, T.Text) Statement
                stmtpmap = foldl' (\mp (ttype,tname,ts) -> Map.insert (ttype,tname) (TopContainer [(T.pack fp, topclass)] ts) mp) Map.empty oktoplevels
            ctlg <- getCatalog (getstatement stmtpmap) gettemplate pdb "test" facts (Just "test/modules") baseNativeTypes
            print ctlg
            case ctlg of
                (Right _, _) -> return ("PASS", True)
                (Left x, y) -> error x

