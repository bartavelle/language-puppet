-- | Contains an Haskell implementation (or mock implementation) of some ruby functions found in puppetlabs modules
module Puppet.Puppetlabs (extFunctions) where

import           Control.Lens
import           Crypto.Hash                      as Crypto
import           Data.ByteString                  (ByteString)
import           Data.Foldable                    (foldlM)
import qualified Data.HashMap.Strict              as HM
import           Data.Monoid
import           Data.Scientific                  as Sci
import           Data.Text                        (Text)
import qualified Data.Text                        as Text
import qualified Data.Text.Encoding               as Text
import           Data.Vector                      (Vector)
import           Formatting                       (scifmt, sformat, (%), (%.))
import qualified Formatting                       as FMT
import           System.Posix.Files               (fileExist)
import           System.Random                    (mkStdGen, randomRs)

import           Puppet.Interpreter.PrettyPrinter ()
import           Puppet.Interpreter.Types
import           Puppet.PP

md5 :: Text -> Text
md5 = Text.decodeUtf8 . digestToHexByteString . (Crypto.hash :: ByteString -> Digest MD5) . Text.encodeUtf8

extFun :: [(FilePath, Text, [PValue] -> InterpreterMonad PValue)]
extFun =  [ ("/postgresql", "postgresql_acls_to_resources_hash", pgAclsToHash)
          , ("/postgresql", "postgresql_password", pgPassword)
          , ("/foreman", "random_password", randomPassword)
          , ("/foreman", "cache_data", mockCacheData)
          ]

-- | Build the map of available ext functions
-- If the ruby file is not found on the local filesystem the record is ignored. This is to avoid potential namespace conflict
extFunctions :: FilePath -> IO (Container ( [PValue] -> InterpreterMonad PValue))
extFunctions modpath = foldlM f HM.empty extFun
  where
    f acc (modname, fname, fn) = do
      test <- testFile modname fname
      if test
         then return $ HM.insert fname fn acc
         else return acc
    testFile modname fname = fileExist (modpath <> modname <> "/lib/puppet/parser/functions/" <> Text.unpack fname <>".rb")

pgPassword :: MonadThrowPos m => [PValue] -> m PValue
pgPassword [PString username, PString pwd] =
    return $ PString $ "md5" <> md5 (pwd <> username)
pgPassword _ = throwPosError "expects 2 string arguments"

-- | The function is pure and always return the same "random" password
randomPassword :: MonadThrowPos m => [PValue] -> m PValue
randomPassword x@[PNumber s] =
  PString . Text.pack . randomChars <$> toInt s
  where
    randomChars n = take n $ randomRs ('a', 'z') (mkStdGen 1)
    toInt :: MonadThrowPos m => Scientific -> m Int
    toInt s = maybe (throwPosError $ "Unable to convert" <+> pretty x <+> "into an int.")
                    return
                    (Sci.toBoundedInteger s)

randomPassword _ = throwPosError "expect one single string arguments"


mockCacheData :: MonadThrowPos m => [PValue] -> m PValue
mockCacheData [_, b] = return b
mockCacheData arg@_ = throwPosError $ "expect 2 string arguments" <+> pretty arg

-- | Simple implemenation that does not handle all cases.
-- For instance 'auth_option' is currently not implemented.
-- Please add cases as needed.
pgAclsToHash :: MonadThrowPos m => [PValue] -> m PValue
pgAclsToHash [PArray as, PString ident, PNumber offset] = do
  x <- aclsToHash as ident offset
  return $ PHash x
pgAclsToHash _ = throwPosError "expects 3 arguments; one array one string and one number"

aclsToHash :: MonadThrowPos m  => Vector PValue -> Text -> Scientific -> m (Container PValue)
aclsToHash vec ident offset = ifoldlM f HM.empty vec
  where
    f :: MonadThrowPos m => Int -> Container PValue -> PValue -> m (Container PValue)
    f idx acc (PString acl) = do
      let order = offset + scientific (toInteger idx) 0
          keymsg = sformat ("postgresql class generated rule " % FMT.stext % " " % FMT.int) ident idx
      x <- aclToHash (Text.words acl) order
      return $ HM.insert keymsg x acc
    f _ _ pval = throwPosError $ "expect a string as acl but get" <+> pretty pval

aclToHash :: (MonadThrowPos m) => [Text] -> Scientific -> m PValue
aclToHash [typ, db, usr, addr, auth] order =
  return $ PHash $ HM.fromList [ ("type", PString typ)
                      , ("database", PString db )
                      , ("user", PString usr)
                      , ("order", PString (sformat (FMT.left 3 '0' %. scifmt Sci.Fixed (Just 0))  order))
                      , ("address", PString addr)
                      , ("auth_method", PString auth)
                      ]
aclToHash acl _ = throwPosError $ "Unable to parse acl line" <+> squotes (ttext (Text.unwords acl))
