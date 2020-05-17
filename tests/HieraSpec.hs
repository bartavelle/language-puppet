module HieraSpec(spec) where

import           XPrelude

import qualified Data.HashMap.Strict as HM
import qualified Data.Vector         as Vector
import qualified System.Log.Logger   as Log
import           Test.Hspec

import           Hiera.Server
import           Puppet.Language

checkOutput :: (Eq a, Show rr, Show a) => a -> Either rr a -> Expectation
checkOutput v (Right x) = x `shouldBe` v
checkOutput _ (Left rr) = expectationFailure (show rr)

checkFail :: (Show a) => Either rr a -> Expectation
checkFail (Right v) = expectationFailure ("Should have failed, but returned: " ++ show v)
checkFail _ = return ()

fqdn :: Text
fqdn = "node.com"

config_v3 :: FilePath
config_v3 = "./tests/hiera/hiera-v3.yaml"

config_v5 :: FilePath
config_v5 = "./tests/hiera/hiera-v5.yaml"

configMisc :: FilePath
configMisc = "./tests/hiera/misc/config/hiera.yaml"

configInterpolate :: FilePath
configInterpolate = "./tests/hiera/interpolate/config/hiera.yaml"

vars :: HM.HashMap Text PValue
vars = HM.fromList [ ("::environment", "production")
                   , ("::fqdn"       , PString fqdn)
                   ]

users :: HM.HashMap Text PValue
users = HM.fromList [ ("pete", PHash (HM.singleton "uid" (PNumber 2000)))
                    , ("tom" , PHash (HM.singleton "uid" (PNumber 2001)))
                    ]

pusers :: HM.HashMap Text PValue
pusers = HM.fromList [ ("bob", PHash (HM.singleton "uid" (PNumber 100)))
                     , ("tom" , PHash (HM.singleton "uid" (PNumber 12)))
                     ]

hash :: [(Text, PValue)] -> PValue
hash = PHash . HM.fromList

array :: [PValue] -> PValue
array = PArray . Vector.fromList

spec :: Spec
spec = do
  runIO $ Log.updateGlobalLogger loggerName (Log.setLevel Log.WARNING)
  q3_ <- runIO $ startHiera "test" config_v3
  q5_ <- runIO $ startHiera "test" config_v5
  let q5 vrs var t = runExceptT (q5_ vrs var t)
      q3 vrs var t = runExceptT (q3_ vrs var t)
  describe "Hiera" $ do
    interpolationSpec
    miscSpec
    describe "v5 lookup hierarchy" $
      it "should override some values"  $ do
        q5 vars "http_port" QFirst >>= checkOutput (Just (PNumber 9090))
        q5 vars "global" QFirst >>= checkOutput (Just "glob")
    describe "v5 ~" $
      it "should read '~' as a Null/Nothing value"  $
        q5 vars "optional_value" QFirst >>= checkOutput (Just PUndef)
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
        q3 mempty "ntp_servers" QFirst >>= checkOutput (Just (array ["0.ntp.puppetlabs.com","1.ntp.puppetlabs.com"]))
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
        q3 vars "testnode" QFirst >>= checkOutput (Just (hash [("1",PString ("**" <> fqdn <> "**")),("2",PString "nothing special")]))
      it "resolves in arrays" $
        q3 vars "arraytest" QFirst >>= checkOutput (Just (array [PString "a", PString fqdn, PString "c"]))
      it "resolves aliases" $
        q3 vars "aliased" QFirst >>= checkOutput (Just (array [PString "a", PString "b"]))
      it "resolves integers to strings" $
        q3 vars "server" QFirst >>= checkOutput (Just (PString "127.0.0.1:9090"))
      it "resolves lookus" $
        q3 vars "aliased_lookup" QFirst >>= checkOutput (Just (PNumber 100))
    describe "v3 other merge modes" $ do
      it "catenates arrays" $
        q3 vars "ntp_servers" QUnique >>= checkOutput (Just (array ["2.ntp.puppetlabs.com","3.ntp.puppetlabs.com","0.ntp.puppetlabs.com","1.ntp.puppetlabs.com"]))
      it "puts single values in arrays" $
        q3 vars "http_port" QUnique >>= checkOutput (Just (array [PNumber 9090, PNumber 8080]))
      it "merges hashes" $
        q3 vars "users" QHash >>= checkOutput (Just (PHash (pusers <> users)))

miscSpec :: Spec
miscSpec = describe "misc" $ do
  q_ <- runIO $ startHiera "test" configMisc
  let q vrs var t = runExceptT (q_ vrs var t)
  it "should return bar on the basic test" $
    q mempty "foo" QFirst >>= checkOutput (Just "bar")
  it "should work with the literal function" $
    q mempty "literal" QFirst >>= checkOutput (Just "%{SERVER_NAME}")

interpolationSpec :: Spec
interpolationSpec =
  describe "interpolation" $ do
    q_ <- runIO $ startHiera "test" configInterpolate
    let q vrs var t = runExceptT (q_ vrs var t)
    describe "when doing interpolation" $ do
      it "should prevent endless recursion" $
        q mempty "foo" QFirst >>= checkFail
      it "produces a nested hash with arrays from nested aliases with hashes and arrays" $
        q mempty "root" QHash >>= checkOutput (Just (hash [("a", hash [("aa", hash [("b", hash [("bb", PArray (Vector.fromList ["text"]))])])])]))
      it "allows keys with white space" $
        q mempty "ws_key" QFirst >>= checkOutput (Just "value for a ws key")
      it "allows keys with non alphanumeric characters" $
        q mempty "angry" QFirst >>= checkOutput (Just "not happy")

    describe "when not finding value for interpolated key" $
      it "should resolve the interpolation to an empty string" $
        q mempty "niltest" QFirst >>= checkFail -- puppet behavior: checkOutput (Just "Missing key ##. Key with nil ##")

    describe "when there are empty interpolations %{} in data" $ do
      it "should should produce an empty string for the interpolation" $ do
        pendingWith "Support empty interpolation"
        q mempty "empty_interpolation" QFirst >>= checkOutput (Just "clownshoe")
      it "the empty interpolation can be escaped" $ do
        pendingWith "Support empty interpolation"
        q mempty "escaped_empty_interpolation" QFirst >>= checkOutput (Just "clown%{shoe}s")
      it "the value can consist of only an empty escape" $ do
        pendingWith "Support empty interpolation"
        q mempty "only_empty_interpolation" QFirst >>= checkOutput (Just "")
      it "the value can consist of an empty namespace %{::}" $ do
        pendingWith "Support empty interpolation"
        q mempty "empty_namespace" QFirst >>= checkOutput (Just "")
      it "the value can consist of whitespace %{ :: }" $ do
        pendingWith "Support empty interpolation"
        q mempty "whitespace1" QFirst >>= checkOutput (Just "")
      it "the value can consist of whitespace %{  }" $ do
        pendingWith "Support empty interpolation"
        q mempty "whitespace2" QFirst >>= checkOutput (Just "")

    describe "when there are quoted empty interpolations %{} in data" $ do
      it "should should produce an empty string for the interpolation" $ do
        pendingWith "Support empty interpolation"
        q mempty "quoted_empty_interpolation" QFirst >>= checkOutput (Just "clownshoe")
      it "the empty interpolation can be escaped" $ do
        pendingWith "Support empty interpolation"
        q mempty "quoted_escaped_empty_interpolation" QFirst >>= checkOutput (Just "clown%{shoe}s")
      it "the value can consist of only an empty escape" $ do
        pendingWith "Support empty interpolation"
        q mempty "quoted_only_empty_interpolation" QFirst >>= checkOutput (Just "")
      it "the value can consist of an empty namespace %{::}" $ do
        pendingWith "Support empty interpolation"
        q mempty "quoted_empty_namespace" QFirst >>= checkOutput (Just "")
      it "the value can consist of whitespace %{ :: }" $ do
        pendingWith "Support empty interpolation"
        q mempty "quoted_whitespace1" QFirst >>= checkOutput (Just "")
      it "the value can consist of whitespace %{  }" $ do
        pendingWith "Support empty interpolation"
        q mempty "quoted_whitespace2" QFirst >>= checkOutput (Just "")

    describe "varsplitter" $ do
      it "no splitting" $ varSplitter "abcd" `shouldBe` HieraVar ("abcd" :| [])
      it "split 2" $ varSplitter "ab.cd" `shouldBe` HieraVar ("ab" :| ["cd"])
      it "split 3" $ varSplitter "ab.cd.ef" `shouldBe` HieraVar ("ab" :| ["cd", "ef"])
      it "split dq" $ varSplitter "\"ab\"" `shouldBe` HieraVar ("ab" :| [])
      it "split dq 2" $ varSplitter "\"ab.cd\"" `shouldBe` HieraVar ("ab.cd" :| [])
      it "split dq 2 mixed" $ varSplitter "\"ab.cd\".ef" `shouldBe` HieraVar ("ab.cd" :| ["ef"])
      it "split dq 2 mixed 4" $ varSplitter "\"ab.cd\".ef.\"lol.cat\".bar" `shouldBe`
            HieraVar ("ab.cd" :| ["ef", "lol.cat", "bar"])
      it "split sq" $ varSplitter "'ab'" `shouldBe` HieraVar ("ab" :| [])
      it "split sq 2" $ varSplitter "'ab.cd'" `shouldBe` HieraVar ("ab.cd" :| [])
      it "split sq 2 mixed" $ varSplitter "'ab.cd'.ef" `shouldBe` HieraVar ("ab.cd" :| ["ef"])
      it "split sq 2 mixed 4" $ varSplitter "'ab.cd'.ef.'lol.cat'.bar" `shouldBe` 
            HieraVar ("ab.cd" :| ["ef", "lol.cat", "bar"])
      it "split all mixed" $ varSplitter "'a.b'.\"c.d\".e.f" `shouldBe`
            HieraVar ("a.b" :| ["c.d", "e", "f"])
      it "function f()" $ varSplitter "f()" `shouldBe`
            HieraFunction ("f" :| []) []
      it "function f('a')" $ varSplitter "f('a')" `shouldBe`
            HieraFunction ("f" :| []) ["a"]
      it "function f('a', 'b', 'c')" $ varSplitter "f('a', 'b', 'c')" `shouldBe`
            HieraFunction ("f" :| []) ["a", "b", "c"]

    describe "when using dotted keys" $ do
      it "should find an entry using a quoted interpolation" $
        q (HM.fromList [("a.b", "(scope) a dot b")]) "\"a.c.scope\"" QFirst >>= checkOutput (Just "a dot c: (scope) a dot b")
      it "should find an entry using a quoted interpolation with method hiera" $
        q (HM.fromList [("a.b", "(scope) a dot b")]) "\"a.c.hiera\"" QFirst >>= checkOutput (Just "a dot c: (hiera) a dot b")
      it "should find an entry using a quoted interpolation with method alias" $
        q (HM.fromList [("a.b", "(scope) a dot b")]) "\"a.c.alias\"" QFirst >>= checkOutput (Just "(hiera) a dot b")
      it "should use a dotted key to navigate into a structure when it is not quoted" $
        q (HM.fromList [("a", hash [("d", "(scope) a dot d is a hash entry")])]) "\"a.e.scope\"" QFirst >>= checkOutput (Just "a dot e: (scope) a dot d is a hash entry")
      it "should use a dotted key to navigate into a structure when when it is not quoted with method hiera" $
        q (HM.fromList [("a", hash [("d", "(scope) a dot d is a hash entry")])]) "\"a.e.hiera\"" QFirst >>= checkOutput (Just "a dot e: (hiera) a dot d is a hash entry")
      it "should use a mix of quoted and dotted keys to navigate into a structure containing dotted keys and quoted key is last" $
        q (HM.fromList [("a", hash [("d.x", "(scope) a dot d.x is a hash entry")])]) "\"a.ex.scope\"" QFirst >>= checkOutput (Just "a dot ex: (scope) a dot d.x is a hash entry")
      it "should use a mix of quoted and dotted keys to navigate into a structure containing dotted keys and quoted key is last and method is hiera" $
        q (HM.fromList [("a", hash [("d.x", "(scope) a dot d.x is a hash entry")])]) "\"a.ex.hiera\"" QFirst >>= checkOutput (Just "a dot ex: (hiera) a dot d.x is a hash entry")
      it "should use a mix of quoted and dotted keys to navigate into a structure containing dotted keys and quoted key is first" $
        q (HM.fromList [("a.x", hash [("d", "(scope) a.x dot d is a hash entry")])]) "\"a.xe.scope\"" QFirst >>= checkOutput (Just "a dot xe: (scope) a.x dot d is a hash entry")
      it "should use a mix of quoted and dotted keys to navigate into a structure containing dotted keys and quoted key is first and method is hiera" $
        q (HM.fromList [("a.x", hash [("d", "(scope) a.x dot d is a hash entry")])]) "\"a.xe.hiera\"" QFirst >>= checkOutput (Just "a dot xe: (hiera) a.x dot d is a hash entry")
      it "should use a mix of quoted and dotted keys to navigate into a structure containing dotted keys and quoted key is in the middle" $
        q (HM.fromList [("a", hash [("d.z", hash [("g", "(scope) a dot d.z dot g is a hash entry")])])]) "\"a.xm.scope\"" QFirst >>= checkOutput (Just "a dot xm: (scope) a dot d.z dot g is a hash entry")
      it "should use a mix of quoted and dotted keys to navigate into a structure containing dotted keys and quoted key is in the middle and method is hiera" $
        q (HM.fromList [("a", hash [("d.z", hash [("g", "(scope) a dot d.z dot g is a hash entry")])])]) "\"a.xm.hiera\"" QFirst >>= checkOutput (Just "a dot xm: (hiera) a dot d.z dot g is a hash entry")
      it "should use a mix of several quoted and dotted keys to navigate into a structure containing dotted keys and quoted key is in the middle" $
        q (HM.fromList [("a.x", hash [("d.z", hash [("g", "(scope) a.x dot d.z dot g is a hash entry")])])]) "\"a.xx.scope\"" QFirst >>= checkOutput (Just "a dot xx: (scope) a.x dot d.z dot g is a hash entry")
      it "should use a mix of several quoted and dotted keys to navigate into a structure containing dotted keys and quoted key is in the middle and method is hiera" $
        q (HM.fromList [("a.x", hash [("d.z", hash [("g", "(scope) a.x dot d.z dot g is a hash entry")])])]) "\"a.xx.hiera\"" QFirst >>= checkOutput (Just "a dot xx: (hiera) a.x dot d.z dot g is a hash entry")
      it "should find an entry using using a quoted interpolation on dotted key containing numbers" $
        q (HM.fromList [("x.1", "(scope) x dot 1")]) "\"x.2.scope\"" QFirst >>= checkOutput (Just "x dot 2: (scope) x dot 1")
      it "should find an entry using using a quoted interpolation on dotted key containing numbers using method hiera" $
        q (HM.fromList [("x.1", "(scope) x dot 1")]) "\"x.2.hiera\"" QFirst >>= checkOutput (Just "x dot 2: (hiera) x dot 1")
      it "will allow strange characters in the key" $
        q mempty "very_angry" QFirst >>= checkOutput (Just "not happy at all")
      it "should not find a subkey when the dotted key is quoted" $
        q (HM.fromList [("a", hash [("f", "(scope) a dot f is a hash entry")])]) "\"a.f.scope\"" QFirst >>= checkFail -- real hiera : checkOutput (Just "a dot f: ")
      it "should not find a subkey when the dotted key is quoted with method hiera" $
        q (HM.fromList [("a", hash [("f", "(scope) a dot f is a hash entry")])]) "\"a.f.hiera\"" QFirst >>= checkFail -- real hiera : checkOutput (Just "a dot f: ")
      it "should not find a subkey that is matched within a string" $
        q mempty "ipl_key" QFirst >>= checkFail
      it "should not find a subkey that is matched within a string" $
        q mempty "key.subkey" QFirst >>= checkFail

