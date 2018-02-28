{-# LANGUAGE OverloadedLists #-}
-- | Publicly exposed pure helpers to evaluate the 'InterpreterMonad'
-- functions that can be found in "Puppet.Interpreter" and
-- "Puppet.Interpreter.Resolve".
--
-- >>> dummyEval (resolveExpression (Addition "1" "2"))
-- Right (PString "3")
module Puppet.Runner.Pure (
    dummyEval
  , dummyFacts
  , pureEval
  , pureEval'
  , pureReader
) where

import           XPrelude

import qualified Data.Either.Strict    as S
import qualified Data.HashMap.Strict   as HM

import           Erb
import           Facter
import           Hiera.Server
import           Puppet.Interpreter
import           Puppet.Parser (Statement)
import           Puppet.Runner.Erb
import           PuppetDB              (dummyPuppetDB)


dummyTemplate :: Monad m => TemplateSource -> InterpreterState -> InterpreterReader m -> m (S.Either PrettyError Text)
dummyTemplate (Filename _) _ _ = return (S.Left "Can't interpret files")
dummyTemplate (Inline cnt) s _ =
  return $ case extractFromState s of
    Nothing -> S.Left "Context retrieval error (pureReader)"
    Just (ctx, scope) ->
      case parseErbString (toS cnt) of
        Left e -> S.Left (PrettyError (pplines (show e)))
        Right stmts ->
          case rubyEvaluate scope ctx stmts of
            Right x -> S.Right x
            Left e  -> S.Left (PrettyError e)

-- | A pure 'InterpreterReader', that can only evaluate a subset of the
-- templates, and that can include only the supplied top level statements.
pureReader :: HM.HashMap (TopLevelType, Text) Statement -- ^ A top-level statement map
           -> InterpreterReader Identity
pureReader sttmap =
  InterpreterReader
    baseNativeTypes
    getstatementdummy
    dummyTemplate
    dummyPuppetDB
    mempty
    "dummy"
    hieradummy
    iomethods_purestubs
    mempty
    mempty
    True
    (puppetPaths "/etc/puppet")
    Nothing
  where
    pure_hiera :: HieraQueryFunc Identity
    pure_hiera _ _ _ = pure (S.Right (Just "pure"))
    hieradummy = HieraQueryLayers pure_hiera mempty
    getstatementdummy tlt n = return $ case HM.lookup (tlt,n) sttmap of
      Just x  -> S.Right x
      Nothing -> S.Left "Can't get statement"
    iomethods_purestubs :: IoMethods Identity
    iomethods_purestubs = IoMethods (return []) (const (return (Left "Can't read file"))) (\_ -> return ())

-- | Evaluates an interpreter expression in a pure context.
pureEval :: Facts -- ^ A list of facts that will be used during evaluation
         ->  HM.HashMap (TopLevelType, Text) Statement -- ^ A top-level map
         -> InterpreterMonad a -- ^ The action to evaluate
         -> (Either PrettyError a, InterpreterState, InterpreterWriter)
pureEval facts stmap action =
  pureEval' stmap state0 action
  where
    state0 = initialState facts $ HM.fromList [("confdir", "/etc/puppet")]

-- | More general version of 'pureEval' where you pass the initial state directly
pureEval' :: HM.HashMap (TopLevelType, Text) Statement -- ^ A top-level map
          -> InterpreterState
          -> InterpreterMonad a -- ^ The action to evaluate
          -> (Either PrettyError a, InterpreterState, InterpreterWriter)
pureEval' stmap s0 action =
  runIdentity (interpretMonad (pureReader stmap) s0 action)


-- | A bunch of facts that can be used for pure evaluation.
dummyFacts :: Facts
dummyFacts = HM.fromList
        [ ("architecture", "amd64")
        , ("augeasversion", "0.10.0")
        , ("bios_release_date", "07/06/2010")
        , ("bios_vendor", "Dell Inc.")
        , ("bios_version", "2.2.0")
        , ("boardmanufacturer", "Dell Inc.")
        , ("domain", "dummy.domain")
        , ("facterversion", "1.7.5")
        , ("filesystems", "ext2,ext3,ext4,vfat")
        , ("fqdn", "dummy.dummy.domain")
        , ("hardwareisa", "x86_64")
        , ("hardwaremodel", "x86_64")
        , ("hostname", "dummy")
        , ("id", "root")
        , ("interfaces", "eth0,lo")
        , ("ipaddress", "172.17.42.1")
        , ("ipaddress_eth0", "172.17.42.1")
        , ("ipaddress_lo", "127.0.0.1")
        , ("is_virtual", "false")
        , ("kernel", "Linux")
        , ("kernelmajversion", "3.8")
        , ("kernelrelease", "3.8.0-37-generic")
        , ("kernelversion", "3.8.0")
        , ("lsbdistcodename", "precise")
        , ("lsbdistdescription", "Ubuntu 12.04.4 LTS")
        , ("lsbdistid", "Ubuntu")
        , ("lsbdistrelease", "12.04")
        , ("lsbmajdistrelease", "12")
        , ("macaddress", "a5:cb:10:b0:9a:4b")
        , ("macaddress_eth0", "72:53:10:c1:eb:70")
        , ("manufacturer", "Dell Inc.")
        , ("memoryfree", "12.57 GB")
        , ("memoryfree_mb", "12869.89")
        , ("memorysize", "15.63 GB")
        , ("memorysize_mb", "16009.07")
        , ("memorytotal", "15.63 GB")
        , ("mtu_eth0", "1500")
        , ("mtu_lo", "65536")
        , ("netmask", "255.255.0.0")
        , ("netmask_eth0", "255.255.255.0")
        , ("netmask_lo", "255.0.0.0")
        , ("network_eth0", "172.17.42.0")
        , ("network_lo", "127.0.0.0")
        , ("operatingsystem", "Ubuntu")
        , ("operatingsystemrelease", "12.04")
        , ("os", PHash [ ("architecture", "amd64")
                       , ("release", PHash [("major", "7")])
                       ])
        , ("osfamily", "Debian")
        , ("path", "/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin")
        , ("processors", PHash [("models", PArray [ "Intel(R) Core(TM) i7 CPU         860  @ 2.80GHz"
                                                  , "Intel(R) Core(TM) i7 CPU         860  @ 2.80GHz"
                                                  , "Intel(R) Core(TM) i7 CPU         860  @ 2.80GHz"
                                                  , "Intel(R) Core(TM) i7 CPU         860  @ 2.80GHz"
                                                  , "Intel(R) Core(TM) i7 CPU         860  @ 2.80GHz"
                                                  , "Intel(R) Core(TM) i7 CPU         860  @ 2.80GHz"
                                                  , "Intel(R) Core(TM) i7 CPU         860  @ 2.80GHz"
                                                  , "Intel(R) Core(TM) i7 CPU         860  @ 2.80GHz"
                                                  , "Intel(R) Core(TM) i7 CPU         860  @ 2.80GHz"
                                                  ])
                               , ("count", "8")
                               , ("physicalprocessorcount", "1")
                               ])
        , ("productname", "Vostro 430")
        , ("ps", "ps -ef")
        , ("puppetversion", "3.4.3")
        , ("rubysitedir", "/usr/local/lib/site_ruby/1.8")
        , ("rubyversion", "1.8.7")
        , ("selinux", "false")
        , ("serialnumber", "9L3FW4J")
        , ("swapfree", "15.96 GB")
        , ("swapfree_mb", "16340.00")
        , ("swapsize", "15.96 GB")
        , ("swapsize_mb", "16340.00")
        , ("timezone", "CEST")
        , ("type", "Desktop")
        , ("uniqueid", "007f0101")
        , ("uptime", "5:48 hours")
        , ("uptime_days", "0")
        , ("uptime_hours", "5")
        , ("uptime_seconds", "20932")
        , ("uuid", "97b75940-be55-11e3-b1b6-0800200c9a66")
        , ("virtual", "physical")
        ]

-- | A default evaluation function for arbitrary interpreter actions.
dummyEval :: InterpreterMonad a -> Either PrettyError a
dummyEval action = pureEval dummyFacts mempty action ^. _1
