{-# LANGUAGE QuasiQuotes #-}
module Main where

import           Helpers

import qualified Data.Either.Strict  as S
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector         as Vector
import           NeatInterpolation
import qualified System.IO.Temp      as IO
import qualified System.Log.Logger   as Log
import           Test.HUnit

import           Hiera.Server

main :: IO ()
main = IO.withSystemTempDirectory "hieratest" $ \tmpfp -> do
    Log.updateGlobalLogger loggerName (Log.setLevel Log.ERROR)
    let ndname = "node.site.com"
        vars = HM.fromList [ ("::environment", "production")
                           , ("::fqdn"       , ndname)
                           ]
        hiera5_config fp =
          [text|
            version: 5
            hierarchy:
              - name: "Hiera config for unit test"
                data_hash: yaml_data
                datadir: $fp
                paths:
                  - "%{::fqdn}.yaml"
                  - "%{::environment}.yaml"
                  - "global.yaml"
          |]
        hiera3_config fp =
          [text|
            :backends:
              - "yaml"
              - "json"
            :logger: "console"
            :hierarchy:
              - "%{::fqdn}"
              - "%{::environment}"
              - "global"
            :yaml:
              :datadir: $fp
            :json:
              :datadir: $fp
          |]
    writeFile (tmpfp <> "/hiera3.yaml") (hiera3_config (toS tmpfp))
    writeFile (tmpfp <> "/hiera5.yaml") (hiera5_config (toS tmpfp))
    writeFile (tmpfp <> "/global.yaml") "---\nhttp_port: 8080\nntp_servers: ['0.ntp.puppetlabs.com', '1.ntp.puppetlabs.com']\nusers:\n  pete:\n    uid: 2000\n  tom:\n    uid: 2001\nglobal: \"glob\""
    writeFile (tmpfp <> "/production.yaml") "---\nhttp_port: 9090\nntp_servers: ['2.ntp.puppetlabs.com', '3.ntp.puppetlabs.com']\ninterp1: '**%{::fqdn}**'\nusers:\n  bob:\n    uid: 100\n  tom:\n    uid: 12\n"
    writeFile (tmpfp <> "/" <> toS ndname <> ".json") "{\"testnode\":{\"1\":\"**%{::fqdn}**\",\"2\":\"nothing special\"},\"testjson\":\"ok\",\"arraytest\":[\"a\",\"%{::fqdn}\",\"c\"]}\n"
    let users = HM.fromList [ ("pete", PHash (HM.singleton "uid" (PNumber 2000)))
                            , ("tom" , PHash (HM.singleton "uid" (PNumber 2001)))
                            ]
        pusers = HM.fromList [ ("bob", PHash (HM.singleton "uid" (PNumber 100)))
                             , ("tom" , PHash (HM.singleton "uid" (PNumber 12)))
                             ]
    q3 <- startHiera (tmpfp ++ "/hiera3.yaml")
    q5 <- startHiera (tmpfp ++ "/hiera5.yaml")
    let checkOutput v (S.Right x) = x @?= v
        checkOutput _ (S.Left rr) = assertFailure (show rr)
    hspec $ do
        describe "lookup data without a key" $
            it "returns an error when called with an empty string" $ q3 mempty "" QFirst >>= checkOutput Nothing
        describe "lookup data without a valid key" $ do
            it "returns an error when called with a non existent key [QFirst]" $ q3 mempty "foo" QFirst >>= checkOutput Nothing
            it "returns an error when called with a non existent key [QUnique]" $ q3 mempty "foo" QUnique >>= checkOutput Nothing
            it "returns an error when called with a non existent key [QHash]" $ q3 mempty "foo" QHash >>= checkOutput Nothing
        describe "lookup data with no options" $ do
            it "can get string data" $ q3 mempty "http_port" QFirst >>= checkOutput (Just (PNumber 8080))
            it "can get arrays" $ q3 mempty "ntp_servers" QFirst >>= checkOutput (Just (PArray (Vector.fromList ["0.ntp.puppetlabs.com","1.ntp.puppetlabs.com"])))
            it "can get hashes" $ q3 mempty "users" QFirst >>= checkOutput (Just (PHash users))
        describe "lookup data with a scope" $ do
            it "overrides some values" $ q3 vars "http_port" QFirst >>= checkOutput (Just (PNumber 9090))
            it "doesn't fail on others" $ q3 vars "global" QFirst >>= checkOutput (Just "glob")
        describe "json backend" $
            it "resolves in json" $ q3 vars "testjson" QFirst >>= checkOutput (Just "ok")
        describe "deep interpolation" $ do
            it "resolves in strings" $ q3 vars "interp1" QFirst >>= checkOutput (Just (PString ("**" <> ndname <> "**")))
            it "resolves in objects" $ q3 vars "testnode" QFirst >>= checkOutput (Just (PHash (HM.fromList [("1",PString ("**" <> ndname <> "**")),("2",PString "nothing special")])))
            it "resolves in arrays" $ q3 vars "arraytest" QFirst >>= checkOutput (Just (PArray (Vector.fromList [PString "a", PString ndname, PString "c"])))
        describe "other merge modes" $ do
            it "catenates arrays" $ q3 vars "ntp_servers" QUnique >>= checkOutput (Just (PArray (Vector.fromList ["2.ntp.puppetlabs.com","3.ntp.puppetlabs.com","0.ntp.puppetlabs.com","1.ntp.puppetlabs.com"])))
            it "puts single values in arrays" $ q3 vars "http_port" QUnique >>= checkOutput (Just (PArray (Vector.fromList [PNumber 9090, PNumber 8080])))
            it "merges hashes" $ q3 vars "users" QHash >>= checkOutput (Just (PHash (pusers <> users)))

        -- V5 format
        describe "[V5] lookup data with a scope" $ do
            it "overrides some values" $ q5 vars "http_port" QFirst >>= checkOutput (Just (PNumber 9090))
            it "doesn't fail on others" $ q5 vars "global" QFirst >>= checkOutput (Just "glob")
