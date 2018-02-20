module PuppetdbSpec(spec) where

import           XPrelude

import qualified Data.Text      as Text
import           System.IO.Temp as Temp
import           Test.Hspec

import           PuppetDB

checkPanicE :: Show x => Text -> ExceptT x IO a -> IO a
checkPanicE msg = runExceptT >=> either (panic . ((msg <> " ") <>) . show) return

fqdn = "node.site.com"

spec :: SpecWith ()
spec =
  around (Temp.withSystemTempDirectory "hieratest") $ do
    describe "PuppetDB" $ do
      it "should save facts" $ \tmpfp -> do
        let pdbfile = tmpfp <> "/puppetdb.yaml"
        -- generate an empty puppetdb
        pdb <- loadTestDB pdbfile >>= unwrapError "While loading a test DB"
        -- get some dummy facts
        facts <- puppetDBFacts fqdn pdb
        -- and add a custom fact
        let nfacts = facts & at "customfact" ?~ "MyCustomFactValue"
        -- save the facts
        checkPanicE "replaceFacts" (replaceFacts pdb [(fqdn, nfacts)])
        checkPanicE "commitDB" (commitDB pdb)
        -- check that our custom fact was indeed saved
        dblines <- (fmap Text.strip . Text.lines) <$> readFile pdbfile
        dblines `shouldContain` ["customfact: MyCustomFactValue"]
        -- initiate a new puppetdbapi
        fpdb <- loadTestDB pdbfile >>= unwrapError "loadTestDB"
        ffacts <- puppetDBFacts fqdn pdb
        ffacts `shouldBe` nfacts
        checkPanicE "replaceCatalog" (replaceCatalog fpdb (generateWireCatalog fqdn mempty mempty))
        checkPanicE "commit 2" (commitDB fpdb)
        -- check our facts again
        fdblines <- (fmap (Text.strip) . Text.lines) `fmap` readFile pdbfile
        fdblines `shouldContain` ["customfact: MyCustomFactValue"]
