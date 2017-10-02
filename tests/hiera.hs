module Main where

import Test.Hspec
import System.IO.Temp
import Hiera.Server
import Data.Monoid
import qualified Data.Either.Strict as S
import Test.HUnit
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified System.Log.Logger as LOG

import Puppet.Interpreter.Types

main :: IO ()
main = withSystemTempDirectory "hieratest" $ \tmpfp -> do
    LOG.updateGlobalLogger hieraLoggerName (LOG.setLevel LOG.ERROR)
    let ndname = "node.site.com"
        vars = HM.fromList [ ("::environment", "production")
                           , ("::fqdn"       , ndname)
                           ]
    writeFile (tmpfp ++ "/hiera.yaml") $ "---\n:backends:\n  - \"yaml\"\n  - \"json\"\n:logger: \"console\"\n:hierarchy:\n  - \"%{::fqdn}\"\n  - \"%{::environment}\"\n  - \"global\"\n\n:yaml:\n  :datadir: " ++ show tmpfp ++ "\n:json:\n  :datadir: " ++ show tmpfp ++ "\n"
    writeFile (tmpfp ++ "/global.yaml") "---\nhttp_port: 8080\nntp_servers: ['0.ntp.puppetlabs.com', '1.ntp.puppetlabs.com']\nusers:\n  pete:\n    uid: 2000\n  tom:\n    uid: 2001\nglobal: \"glob\""
    writeFile (tmpfp ++ "/production.yaml") "---\nhttp_port: 9090\nntp_servers: ['2.ntp.puppetlabs.com', '3.ntp.puppetlabs.com']\ninterp1: '**%{::fqdn}**'\nusers:\n  bob:\n    uid: 100\n  tom:\n    uid: 12\n"
    writeFile (tmpfp ++ "/" ++ T.unpack ndname ++ ".json") "{\"testnode\":{\"1\":\"**%{::fqdn}**\",\"2\":\"nothing special\"},\"testjson\":\"ok\",\"arraytest\":[\"a\",\"%{::fqdn}\",\"c\"]}\n"
    let users = HM.fromList [ ("pete", PHash (HM.singleton "uid" (PNumber 2000)))
                            , ("tom" , PHash (HM.singleton "uid" (PNumber 2001)))
                            ]
        pusers = HM.fromList [ ("bob", PHash (HM.singleton "uid" (PNumber 100)))
                             , ("tom" , PHash (HM.singleton "uid" (PNumber 12)))
                             ]
    Right q <- startHiera (tmpfp ++ "/hiera.yaml")
    let checkOutput v (S.Right x) = x @?= v
        checkOutput _ (S.Left rr) = assertFailure (show rr)
    hspec $ do
        describe "lookup data without a key" $
            it "returns an error when called with an empty string" $ q mempty "" QFirst >>= checkOutput Nothing
        describe "lookup data without a valid key" $ do
            it "returns an error when called with a non existent key [QFirst]" $ q mempty "foo" QFirst >>= checkOutput Nothing
            it "returns an error when called with a non existent key [QUnique]" $ q mempty "foo" QUnique >>= checkOutput Nothing
            it "returns an error when called with a non existent key [QHash]" $ q mempty "foo" QHash >>= checkOutput Nothing
        describe "lookup data with no options" $ do
            it "can get string data" $ q mempty "http_port" QFirst >>= checkOutput (Just (PNumber 8080))
            it "can get arrays" $ q mempty "ntp_servers" QFirst >>= checkOutput (Just (PArray (V.fromList ["0.ntp.puppetlabs.com","1.ntp.puppetlabs.com"])))
            it "can get hashes" $ q mempty "users" QFirst >>= checkOutput (Just (PHash users))
        describe "lookup data with a scope" $ do
            it "overrides some values" $ q vars "http_port" QFirst >>= checkOutput (Just (PNumber 9090))
            it "doesn't fail on others" $ q vars "global" QFirst >>= checkOutput (Just "glob")
        describe "json backend" $
            it "resolves in json" $ q vars "testjson" QFirst >>= checkOutput (Just "ok")
        describe "deep interpolation" $ do
            it "resolves in strings" $ q vars "interp1" QFirst >>= checkOutput (Just (PString ("**" <> ndname <> "**")))
            it "resolves in objects" $ q vars "testnode" QFirst >>= checkOutput (Just (PHash (HM.fromList [("1",PString ("**" <> ndname <> "**")),("2",PString "nothing special")])))
            it "resolves in arrays" $ q vars "arraytest" QFirst >>= checkOutput (Just (PArray (V.fromList [PString "a", PString ndname, PString "c"])))
        describe "other merge modes" $ do
            it "catenates arrays" $ q vars "ntp_servers" QUnique >>= checkOutput (Just (PArray (V.fromList ["2.ntp.puppetlabs.com","3.ntp.puppetlabs.com","0.ntp.puppetlabs.com","1.ntp.puppetlabs.com"])))
            it "puts single values in arrays" $ q vars "http_port" QUnique >>= checkOutput (Just (PArray (V.fromList [PNumber 9090, PNumber 8080])))
            it "merges hashes" $ q vars "users" QHash >>= checkOutput (Just (PHash (pusers <> users)))
