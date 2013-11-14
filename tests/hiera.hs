module Main where

import Test.Hspec
import System.IO.Temp
import Hiera.Server
import Data.Monoid
import qualified Data.Either.Strict as S
import qualified Data.Maybe.Strict as S
import Data.Tuple.Strict
import Test.HUnit
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

import Puppet.Parser.Types
import Puppet.Interpreter.Types

main :: IO ()
main = withSystemTempDirectory "hieratest" $ \tmpfp -> do
    let ndname = "node.site.com"
        newscope = HM.singleton "::" (ScopeInformation vars mempty mempty (CurContainer ContRoot mempty) mempty S.Nothing)
        vars = HM.fromList [ ("environment", ("production" :!: initialPPos "dummy" :!: ContRoot))
                           , ("fqdn"       , (PString ndname :!: initialPPos "dummy" :!: ContRoot))
                           ]
    writeFile (tmpfp ++ "/hiera.yaml") $ "---\n:backends:\n  - \"yaml\"\n  - \"json\"\n:logger: \"console\"\n:hierarchy:\n  - \"%{::fqdn}\"\n  - \"%{::environment}\"\n  - \"global\"\n\n:yaml:\n  :datadir: " ++ show tmpfp ++ "\n:json:\n  :datadir: " ++ show tmpfp ++ "\n"
    writeFile (tmpfp ++ "/global.yaml") "---\nhttp_port: 8080\nntp_servers: ['0.ntp.puppetlabs.com', '1.ntp.puppetlabs.com']\nusers:\n  pete:\n    uid: 2000\n  tom:\n    uid: 2001\n"
    writeFile (tmpfp ++ "/production.yaml") "---\nhttp_port: 9090\nntp_servers: ['2.ntp.puppetlabs.com', '3.ntp.puppetlabs.com']\ninterp1: '**%{::fqdn}**'\n"
    writeFile (tmpfp ++ "/" ++ T.unpack ndname ++ ".json") "{\"testnode\":{\"1\":\"**%{::fqdn}**\",\"2\":\"nothing special\"},\"testjson\":\"ok\",\"arraytest\":[\"a\",\"%{::fqdn}\",\"c\"]}\n"
    let users = HM.fromList [ ("pete", PHash (HM.singleton "uid" "2000"))
                            , ("tom" , PHash (HM.singleton "uid" "2001"))
                            ]
    Right q <- startHiera (tmpfp ++ "/hiera.yaml")
    let checkOutput v (S.Right x) = x @?= v
        checkOutput _ (S.Left rr) = assertFailure (show rr)
    hspec $ do
        describe "lookup data without a key" $ do
            it "returns an error when called with an empty string" $ q mempty "" >>= checkOutput S.Nothing
        describe "lookup data with no options" $ do
            it "can get string data" $ q mempty "http_port" >>= checkOutput (S.Just "8080")
            it "can get arrays" $ q mempty "ntp_servers" >>= checkOutput (S.Just (PArray (V.fromList ["0.ntp.puppetlabs.com","1.ntp.puppetlabs.com"])))
            it "can get hashes" $ q mempty "users" >>= checkOutput (S.Just (PHash users))
        describe "lookup data with a scope" $ do
            it "overrides some values" $ q newscope "http_port" >>= checkOutput (S.Just "9090")
            it "doesn't fail on others" $ q newscope "users" >>= checkOutput (S.Just (PHash users))
        describe "json backend" $ do
            it "resolves in json" $ q newscope "testjson" >>= checkOutput (S.Just "ok")
        describe "deep interpolation" $ do
            it "resolves in strings" $ q newscope "interp1" >>= checkOutput (S.Just (PString ("**" <> ndname <> "**")))
            it "resolves in objects" $ q newscope "testnode" >>= checkOutput (S.Just (PHash (HM.fromList [("1",PString ("**" <> ndname <> "**")),("2",PString "nothing special")])))
            it "resolves in arrays" $ q newscope "arraytest" >>= checkOutput (S.Just (PArray (V.fromList [PString "a", PString ndname, PString "c"])))

