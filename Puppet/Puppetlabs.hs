-- | Contains an Haskell implementation of some ruby functions found in puppetlabs modules
-- There is currently no attempt for solving namespacing and implementation might be gross.
module Puppet.Puppetlabs where

import           Control.Lens
import qualified Data.HashMap.Strict              as HM
import           Data.Monoid
import           Data.Scientific                  as Scientific
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           Data.Vector                      (Vector)
import           Formatting                       (left, scifmt, sformat, (%.))

import           Puppet.Interpreter.PrettyPrinter ()
import           Puppet.Interpreter.Types
import           Puppet.PP

extFunctions :: Container ( [PValue] -> InterpreterMonad PValue )
extFunctions = HM.fromList [("postgresql_acls_to_resources_hash", pgAclsToHash)
                           ]
-- | Simple implemenation that does not handle all cases.
-- For instance 'auth_option' is currently not implemented.
-- Please add cases as needed.
pgAclsToHash :: MonadThrowPos m => [PValue] -> m PValue
pgAclsToHash [PArray as, PString ident, PNumber offset] = do
  hash <- aclsToHash as ident offset
  return $ PHash hash
pgAclsToHash _ = throwPosError "expects 3 arguments; one array one string and one number"

aclsToHash :: MonadThrowPos m  => Vector PValue -> Text -> Scientific -> m (Container PValue)
aclsToHash v ident offset = ifoldlM f HM.empty v
  where
    f :: MonadThrowPos m => Int -> Container PValue -> PValue -> m (Container PValue)
    f idx acc (PString acl) = do
      let order = offset + scientific (toInteger idx) 0
      hash <- aclToHash (T.words acl) order
      return $ HM.insert ("postgresql class generated rule " <> ident <> " " <> tshow idx) hash acc
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
aclToHash acl _ = throwPosError $ "Unable to parse acl line" <+> squotes (ttext (T.unwords acl))
