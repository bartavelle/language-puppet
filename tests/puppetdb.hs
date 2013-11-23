module Main where

import Control.Monad
import System.IO.Temp
import Data.Monoid
import qualified Data.Either.Strict as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Lens

import Puppet.Interpreter.Types
import PuppetDB.Common
import PuppetDB.TestDB
import Facter

checkError :: Show x => String -> S.Either x a -> IO a
checkError _ (S.Right x) = return x
checkError step (S.Left rr) = error (step ++ ": " ++ show rr)

main :: IO ()
main = withSystemTempDirectory "hieratest" $ \tmpfp -> do
    let ndname = "node.site.com"
        pdbfile = tmpfp <> "/puppetdb.yaml"
    -- generate an empty puppetdb
    pdb <- loadTestDB pdbfile >>= checkError "loadTestDB"
    -- get some dummy facts
    facts <- puppetDBFacts ndname pdb
    -- and add a custom fact
    let nfacts = facts & at "customfact" ?~ "MyCustomFactValue"
    -- save the facts
    replaceFacts pdb [(ndname, nfacts)] >>= checkError "replaceFacts"
    commitDB pdb >>= checkError "commitDB"
    -- check that our custom fact was indeed saved
    dblines <- T.lines `fmap` T.readFile pdbfile
    unless ("    customfact: MyCustomFactValue" `elem` dblines) (error "could not find my fact")
    -- now we initiate a new puppetdbapi
    fpdb <- loadTestDB pdbfile >>= checkError "loadTestDB"
    ffacts <- puppetDBFacts ndname pdb
    unless (ffacts == nfacts) (error "facts are distinct")
    replaceCatalog fpdb (generateWireCatalog ndname mempty mempty) >>= checkError "replaceCatalog"
    commitDB fpdb >>= checkError "commit 2"
    -- and check for our facts again
    fdblines <- T.lines `fmap` T.readFile pdbfile
    unless ("    customfact: MyCustomFactValue" `elem` fdblines) (error "could not find my fact")

