{-# LANGUAGE OverloadedLists #-}
-- | Contains an Haskell implementation (or mock implementation) of some ruby functions found in puppetlabs modules.
module Puppet.Runner.Puppetlabs (extFunctions) where

import           XPrelude

import           Crypto.Hash         as Crypto
import qualified Data.HashMap.Strict as Map
import           Data.Scientific     as Sci
import qualified Data.Text           as Text
import qualified Data.Text.Encoding  as Text
import           Formatting          (scifmt, sformat, (%), (%.))
import qualified Formatting          as FMT
import qualified System.Directory    as Directory
import           System.FilePath     ((<.>), (</>))
import           System.Random       (mkStdGen, randomRs)

import           Puppet.Interpreter

md5 :: Text -> Text
md5 = Text.pack . show . (Crypto.hash :: ByteString -> Digest MD5) . Text.encodeUtf8

extFun :: [(Text, Text, [PValue] -> InterpreterMonad PValue)]
extFun =  [ ("apache", "bool2httpd", apacheBool2httpd)
          , ("docker", "docker_swarm_join_flags", mockDockerSwarmJoinFlags)
          , ("docker", "docker_swarm_init_flags", mockDockerSwarmInitFlags)
          , ("docker", "docker_run_flags", mockDockerRunFlags)
          , ("docker", "docker_stack_flags", mockDockerStackFlags)
          , ("docker", "docker_secrets_flags", mockDockerSecretsFlags)
          , ("docker", "sanitised_name", dockerSanitisedName)
          , ("jenkins", "jenkins_port", mockJenkinsPort)
          , ("jenkins", "jenkins_prefix", mockJenkinsPrefix)
          , ("postgresql", "postgresql_acls_to_resources_hash", pgAclsToHash)
          , ("postgresql", "postgresql_password", pgPassword)
          , ("puppetdb", "puppetdb_create_subsetting_resource_hash", puppetdbCreateSubsettingResourceHash)
          , ("extlib", "random_password", randomPassword)
          , ("extlib", "cache_data", mockCacheData)
          , ("kubernetes", "kubeadm_init_flags", mockKubernetesInitFlags)
          , ("kubernetes", "kubeadm_join_flags", mockKubernetesJoinFlags)
          ]

-- | Build the map of available external functions.
--
-- If the ruby/puppet file is not found on the local filesystem the record is ignored. This is to avoid potential namespace conflict.
extFunctions :: FilePath -> IO (Container ( [PValue] -> InterpreterMonad PValue))
extFunctions modpath = foldlM f Map.empty extFun
  where
    f acc (nsp, name, fn) = do
      test <- testFile (toS nsp) name
      if test
         then pure $ Map.insert name fn acc
         else pure acc
    testFile nspath funcname =
      let funcpath0 = modpath </> nspath
          funcpath1 = funcpath0 </> "lib/puppet"
          funcpath2 = funcpath1 </> "parser/functions"
          funcpath3 = funcpath1 </> "functions"
      in
      (\a b -> isJust a || isJust b)
        <$> Directory.findFile [ funcpath0 </> "functions"] (toS funcname <.> "pp")
        <*> Directory.findFile [ funcpath2
                               , funcpath3
                               , funcpath2 </> nspath
                               , funcpath3 </> nspath
                               ] (toS funcname <.> "rb")

apacheBool2httpd :: MonadThrowPos m => [PValue] -> m PValue
apacheBool2httpd [PBoolean True]  = pure $ PString "On"
apacheBool2httpd [PString "true"] = pure $ PString "On"
apacheBool2httpd [_]              = pure $ PString "Off"
apacheBool2httpd arg            = throwPosError $ "expect one single argument" <+> pretty arg

pgPassword :: MonadThrowPos m => [PValue] -> m PValue
pgPassword [PString username, PString pwd] =
    return $ PString $ "md5" <> md5 (pwd <> username)
pgPassword _ = throwPosError "expects 2 string arguments"

-- | The function is pure and always return the same "random" password.
randomPassword :: MonadThrowPos m => [PValue] -> m PValue
randomPassword [PNumber s] =
  PString . Text.pack . randomChars <$> scientificToInt s
  where
    randomChars n = take n $ randomRs ('a', 'z') (mkStdGen 1)

randomPassword _ = throwPosError "expect one single string arguments"


-- To be implemented if needed.
mockJenkinsPrefix :: MonadThrowPos m => [PValue] -> m PValue
mockJenkinsPrefix []    = return $ PString ""
mockJenkinsPrefix arg = throwPosError $ "expect no argument" <+> pretty arg

-- To be implemented if needed.
mockJenkinsPort :: MonadThrowPos m => [PValue] -> m PValue
mockJenkinsPort []    = return $ PString "8080"
mockJenkinsPort arg = throwPosError $ "expect no argument" <+> pretty arg

mockCacheData :: MonadThrowPos m => [PValue] -> m PValue
mockCacheData [_, _, b] = return b
mockCacheData arg     = throwPosError $ "expect 3 string arguments" <+> pretty arg

-- | Simple implemenation that does not handle all cases.
-- For instance 'auth_option' is currently not implemented.
-- Please add cases as needed.
pgAclsToHash :: MonadThrowPos m => [PValue] -> m PValue
pgAclsToHash [PArray as, PString ident, PNumber offset] = PHash <$> aclsToHash as ident offset
pgAclsToHash _ = throwPosError "expects 3 arguments; one array one string and one number"

aclsToHash :: MonadThrowPos m  => Vector PValue -> Text -> Scientific -> m (Container PValue)
aclsToHash vec ident offset = ifoldlM f Map.empty vec
  where
    f :: MonadThrowPos m => Int -> Container PValue -> PValue -> m (Container PValue)
    f idx acc (PString acl) = do
      let order = offset + scientific (toInteger idx) 0
          keymsg = sformat ("postgresql class generated rule " % FMT.stext % " " % FMT.int) ident idx
      x <- aclToHash (Text.words acl) order
      return $ Map.insert keymsg x acc
    f _ _ pval = throwPosError $ "expect a string as acl but get" <+> pretty pval

aclToHash :: (MonadThrowPos m) => [Text] -> Scientific -> m PValue
aclToHash acl@(typ : db : usr : remaining) order = analyze
  where
    fin remn hs = return $ PHash $
        if null remn
          then hs
          else Map.insert "auth_option" (PString (Text.unwords remn)) hs
    analyze = case remaining of
                method : remn | typ == "local" ->
                  fin remn $ baseHash & at "auth_method" ?~ PString method
                addr : msk : method : remn | Text.all isDigit msk ->
                  fin remn $ baseHash & at "address" ?~ PString (Text.unwords [addr,msk])
                                      & at "auth_method" ?~ PString method
                addr : method : remn ->
                  fin remn $ baseHash & at "address" ?~ PString addr
                                      & at "auth_method" ?~ PString method
                _ -> throwPosError $ "Unable to parse acl line" <+> squotes (ppline (Text.unwords acl))
    baseHash = [ ("type", PString "local")
               , ("database", PString db )
               , ("user", PString usr)
               , ("order", PString (sformat (FMT.left 3 '0' %. scifmt Sci.Fixed (Just 0))  order))
               ]
aclToHash acl _ = throwPosError $ "Unable to parse acl line" <+> squotes (ppline (Text.unwords acl))

-- faked implementation, replace by the correct one if you need so.
mockDockerRunFlags :: MonadThrowPos m => [PValue] -> m PValue
mockDockerRunFlags arg@[PHash _]= (pure . PString . show . head) arg
mockDockerRunFlags  arg = throwPosError $ "Expect an hash as argument but was" <+> pretty arg

-- faked implementation, replace by the correct one if you need so.
mockDockerStackFlags :: MonadThrowPos m => [PValue] -> m PValue
mockDockerStackFlags arg@[PHash _]= (pure . PString . show . head) arg
mockDockerStackFlags  arg = throwPosError $ "Expect an hash as argument but was" <+> pretty arg

-- faked implementation, replace by the correct one if you need so.
mockDockerSecretsFlags :: MonadThrowPos m => [PValue] -> m PValue
mockDockerSecretsFlags arg@[PHash _]= (pure . PString . show . head) arg
mockDockerSecretsFlags  arg = throwPosError $ "Expect an hash as argument but was" <+> pretty arg

-- faked implementation, replace by the correct one if you need so.
mockDockerSwarmJoinFlags :: MonadThrowPos m => [PValue] -> m PValue
mockDockerSwarmJoinFlags arg@[PHash _]= (pure . PString . show . head) arg
mockDockerSwarmJoinFlags  arg = throwPosError $ "Expect an hash as argument but was" <+> pretty arg

-- faked implementation, replace by the correct one if you need so.
mockDockerSwarmInitFlags :: MonadThrowPos m => [PValue] -> m PValue
mockDockerSwarmInitFlags arg@[PHash _]= (pure . PString . show . head) arg
mockDockerSwarmInitFlags  arg = throwPosError $ "Expect an hash as argument but was" <+> pretty arg

-- faked implementation, replace by the correct one if you need so.
mockKubernetesInitFlags :: MonadThrowPos m => [PValue] -> m PValue
mockKubernetesInitFlags arg@[PHash _]= (pure . PString . show . head) arg
mockKubernetesInitFlags  arg = throwPosError $ "Expect an hash as argument but was" <+> pretty arg

-- faked implementation, replace by the correct one if you need so.
mockKubernetesJoinFlags :: MonadThrowPos m => [PValue] -> m PValue
mockKubernetesJoinFlags arg@[PHash _]= (pure . PString . show . head) arg
mockKubernetesJoinFlags  arg = throwPosError $ "Expect an hash as argument but was" <+> pretty arg

-- utils
scientificToInt :: MonadThrowPos m => Scientific -> m Int
scientificToInt s = maybe (throwPosError $ "Unable to convert" <+> pretty s <+> "into an int.")
                          pure
                          (Sci.toBoundedInteger s)

-- https://github.com/puppetlabs/puppetlabs-puppetdb/blob/master/lib/puppet/parser/functions/puppetdb_create_subsetting_resource_hash.rb
puppetdbCreateSubsettingResourceHash :: MonadThrowPos m => [PValue] -> m PValue
puppetdbCreateSubsettingResourceHash [PHash s, PHash args] = do
  let res_hash = [ (k, PHash h)
                 | (k,v) <- itoList s
                 , let h = [ ( "subsetting", PString k) , ("value", v)] `Map.union` args
                 ]
  pure $ PHash (Map.fromList res_hash)
puppetdbCreateSubsettingResourceHash arg = throwPosError $ "Expect 2 hashes as arguments but was" <+> pretty arg

-- To be implemented if needed.
dockerSanitisedName :: MonadThrowPos m => [PValue] -> m PValue
dockerSanitisedName [PString s] =
  -- ruby implementation: regsubst($name, '[^0-9A-Za-z.\-_]', '-', 'G')
  pure $ PString s
dockerSanitisedName arg = throwPosError $ "Expect an hash as argument but was" <+> pretty arg
