{-# LANGUAGE QuasiQuotes #-}
module HieraSpec(spec) where

import           XPrelude

import qualified Data.Either.Strict  as S
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector         as Vector
import qualified System.Log.Logger   as Log
import           Test.Hspec

import           Hiera.Server
import           Puppet.Language

checkOutput v (S.Right x) = x `shouldBe` v
checkOutput _ (S.Left rr) = expectationFailure (show rr)

fqdn = "node.com"
config_v3 = "./tests/hiera/hiera-v3.yaml"
config_v5 = "./tests/hiera/hiera-v5.yaml"

vars = HM.fromList [ ("::environment", "production")
                   , ("::fqdn"       , fqdn)
                   ]
users = HM.fromList [ ("pete", PHash (HM.singleton "uid" (PNumber 2000)))
                    , ("tom" , PHash (HM.singleton "uid" (PNumber 2001)))
                    ]
pusers = HM.fromList [ ("bob", PHash (HM.singleton "uid" (PNumber 100)))
                     , ("tom" , PHash (HM.singleton "uid" (PNumber 12)))
                     ]

spec = do
  runIO $ Log.updateGlobalLogger loggerName (Log.setLevel Log.WARNING)
  q3 <- runIO $ startHiera "test" config_v3
  q5 <- runIO $ startHiera "test" config_v5
  describe "Hiera" $ do
  describe "v5 lookup hierarchy" $ do
    it "should override some values"  $ do
      q5 vars "http_port" QFirst >>= checkOutput (Just (PNumber 9090))
      q5 vars "global" QFirst >>= checkOutput (Just "glob")
  describe "v5 ~" $ do
    it "should read '~' as a Null/Nothing value"  $ do
      q5 vars "optional_value" QFirst >>= checkOutput Nothing
  describe "v3 lookup with no context variables" $ do
    it "should return nothing when called with an empty string" $
      q3 mempty "" QFirst >>= checkOutput Nothing
    it "should return nothing when called with a non existent key [First merge]" $
      q3 mempty "foo" QFirst >>= checkOutput Nothing
    it "should return nothing when called with a non existent key [Unique merge]" $
      q3 mempty "foo" QUnique >>= checkOutput Nothing
    it "should return nothing when called with a non existent key [Hash merge]" $
      q3 mempty "foo" QHash >>= checkOutput Nothing
    it "should return common data" $
      q3 mempty "http_port" QFirst >>= checkOutput (Just (PNumber 8080))
    it "should return arrays" $
      q3 mempty "ntp_servers" QFirst >>= checkOutput (Just (PArray (Vector.fromList ["0.ntp.puppetlabs.com","1.ntp.puppetlabs.com"])))
    it "should return hashes" $
      q3 mempty "users" QFirst >>= checkOutput (Just (PHash users))
  describe "v3 lookup hierarchy" $ do
    it "should override value" $
      q3 vars "http_port" QFirst >>= checkOutput (Just (PNumber 9090))
    it "should find common value" $
      q3 vars "global" QFirst >>= checkOutput (Just "glob")
  describe "v3 json backend" $
    it "resolves in json" $
      q3 vars "testjson" QFirst >>= checkOutput (Just "ok")
  describe "v3 deep interpolation" $ do
    it "resolves in strings" $
      q3 vars "interp1" QFirst >>= checkOutput (Just (PString ("**" <> fqdn <> "**")))
    it "resolves in objects" $
      q3 vars "testnode" QFirst >>= checkOutput (Just (PHash (HM.fromList [("1",PString ("**" <> fqdn <> "**")),("2",PString "nothing special")])))
    it "resolves in arrays" $
      q3 vars "arraytest" QFirst >>= checkOutput (Just (PArray (Vector.fromList [PString "a", PString fqdn, PString "c"])))
  describe "v3 other merge modes" $ do
    it "catenates arrays" $
      q3 vars "ntp_servers" QUnique >>= checkOutput (Just (PArray (Vector.fromList ["2.ntp.puppetlabs.com","3.ntp.puppetlabs.com","0.ntp.puppetlabs.com","1.ntp.puppetlabs.com"])))
    it "puts single values in arrays" $
      q3 vars "http_port" QUnique >>= checkOutput (Just (PArray (Vector.fromList [PNumber 9090, PNumber 8080])))
    it "merges hashes" $
      q3 vars "users" QHash >>= checkOutput (Just (PHash (pusers <> users)))
