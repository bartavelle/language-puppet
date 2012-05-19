module Puppet.NativeTypes.Helpers where

import Puppet.Interpreter.Types
import qualified Data.Map as Map

type PuppetTypeName = String
type PuppetTypeValidate = RResource -> Either String RResource
type PuppetTypeMethods = (PuppetTypeValidate)

faketype :: PuppetTypeName -> (PuppetTypeName, PuppetTypeMethods)
faketype tname = (tname, \x -> Right x)

defaulttype :: PuppetTypeName -> (PuppetTypeName, PuppetTypeMethods)
defaulttype tname = (tname, defaultValidate)

-- this helper validates any resource
-- it fills the name and title parameter if necessary
defaultValidate :: PuppetTypeValidate
defaultValidate res = Right (res { rrparams = newparams } )
    where
        newparams = Map.union defaults  (rrparams res) 
        defaults  = Map.fromList [("name", nm),("title", nm)]
        nm = ResolvedString $ rrname res
