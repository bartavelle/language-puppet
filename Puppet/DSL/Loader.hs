module Puppet.DSL.Loader (loadClass) where

import Puppet.DSL.Types
import qualified Data.Map as Map
import Debug.Trace

-- contrary to what the name says, it also grabs defines
getTopClasses statements = Map.fromList $ map toAssocList (filter classOrDefine statements)
    where
        classOrDefine (ClassDeclaration _ _ _ _ _) = True
        classOrDefine (DefineDeclaration _ _ _ _) = True
        classOrDefine _ = False
        toAssocList (ClassDeclaration name parent params content pos) = (name, ClassDeclaration name parent params content pos)
        toAssocList (DefineDeclaration name params content pos) = (name, DefineDeclaration name params content pos)

loadPuppetClass :: Preferences -> String -> [Statement]
loadPuppetClass (Preferences pDir mDir _) classname = trace ("load " ++ classname) []

loadClass :: Preferences -> String -> Map.Map String Statement
loadClass prefs classname = getTopClasses $ loadPuppetClass prefs classname
