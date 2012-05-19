module Puppet.NativeTypes.Helpers where

import Puppet.Interpreter.Types

type PuppetTypeName = String
type PuppetTypeValidate = RResource -> Either String RResource
type PuppetTypeMethods = (PuppetTypeValidate)

faketype :: PuppetTypeName -> (PuppetTypeName, PuppetTypeMethods)
faketype tname = (tname, \x -> Right x)

defaulttype :: PuppetTypeName -> (PuppetTypeName, PuppetTypeMethods)
defaulttype tname = (tname, defaultValidate)

defaultValidate :: PuppetTypeValidate
defaultValidate res = Right res
--(res { rrparams = ((rrparams res) ++ name ++ title) } )
