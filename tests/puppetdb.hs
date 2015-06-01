module Main where

import Control.Monad
import System.IO.Temp
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Lens
import Control.Monad.Trans.Either

import Puppet.Interpreter.Types
import PuppetDB.Common
import PuppetDB.TestDB
import Facter

checkError :: Show x => String -> Either x a -> IO a
checkError _ (Right x) = return x
checkError step (Left rr) = error (step ++ ": " ++ show rr)

checkErrorE :: Show x => String -> EitherT x IO a -> IO a
checkErrorE msg = runEitherT >=> either (error . ((msg ++ " ") ++) . show) return

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
    checkErrorE "replaceFacts" (replaceFacts pdb [(ndname, nfacts)])
    checkErrorE "commitDB" (commitDB pdb)
    -- check that our custom fact was indeed saved
    dblines <- T.lines `fmap` T.readFile pdbfile
    unless ("    customfact: MyCustomFactValue" `elem` dblines) (error "could not find my fact")
    -- now we initiate a new puppetdbapi
    fpdb <- loadTestDB pdbfile >>= checkError "loadTestDB"
    ffacts <- puppetDBFacts ndname pdb
    unless (ffacts == nfacts) (error "facts are distinct")
    checkErrorE "replaceCatalog" (replaceCatalog fpdb (generateWireCatalog ndname mempty mempty))
    checkErrorE "commit 2" (commitDB fpdb)
    -- and check for our facts again
    fdblines <- T.lines `fmap` T.readFile pdbfile
    unless ("    customfact: MyCustomFactValue" `elem` fdblines) (error "could not find my fact")
