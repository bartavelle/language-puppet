-- | Contains an Haskell implementation of some ruby functions found in puppetlabs modules
module Puppet.Puppetlabs (extFunctions) where

import           Control.Lens
import           Data.Foldable                    (foldlM)
import qualified Data.HashMap.Strict              as HM
import           Data.Monoid
import           Data.Scientific                  as Scientific
import           Data.Text                        (Text)
import qualified Data.Text                        as Text
import qualified Data.Text.Encoding as Text
import           Data.Vector                      (Vector)
import           Formatting                       (left, scifmt, sformat, (%.))
import           System.Posix.Files               (fileExist)

import           Puppet.Interpreter.PrettyPrinter ()
import           Puppet.Interpreter.Types
import           Puppet.PP
import Data.ByteString (ByteString)
import Crypto.Hash as Crypto

md5 :: Text -> Text
md5 = Text.decodeUtf8 . digestToHexByteString . (Crypto.hash :: ByteString -> Digest MD5) . Text.encodeUtf8

extFun :: [(FilePath, Text, [PValue] -> InterpreterMonad PValue)]
extFun =  [ ("/postgresql", "postgresql_acls_to_resources_hash", pgAclsToHash)
          , ("/postgresql", "postgresql_password", pgPassword)
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
      x <- aclToHash (Text.words acl) order
      return $ HM.insert ("postgresql class generated rule " <> ident <> " " <> tshow idx) x acc
    f _ _ pval = throwPosError $ "expect a string as acl but get" <+> pretty pval

aclToHash :: (MonadThrowPos m) => [Text] -> Scientific -> m PValue
aclToHash [typ, db, usr, addr, auth] order =
  return $ PHash $ HM.fromList [ ("type", PString typ)
                      , ("database", PString db )
                      , ("user", PString usr)
                      , ("order", PString (sformat (left 3 '0' %. scifmt Scientific.Fixed (Just 0))  order))
                      , ("address", PString addr)
                      , ("auth_method", PString auth)
                      ]
aclToHash acl _ = throwPosError $ "Unable to parse acl line" <+> squotes (ttext (Text.unwords acl))
