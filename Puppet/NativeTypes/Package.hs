{-# LANGUAGE DeriveGeneric #-}
module Puppet.NativeTypes.Package (nativePackage) where

import Puppet.NativeTypes.Helpers
import Puppet.Interpreter.Types
import Control.Monad.Error
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Control.Lens
import GHC.Generics
import Data.Hashable

nativePackage :: (PuppetTypeName, PuppetTypeMethods)
nativePackage = ("package", ptypemethods parameterfunctions (getFeature >=> checkFeatures))

data PackagingFeatures = Holdable | InstallOptions | Installable | Purgeable | UninstallOptions | Uninstallable | Upgradeable | Versionable deriving (Show, Eq, Generic)

instance Hashable PackagingFeatures

isFeatureSupported :: HM.HashMap T.Text (HS.HashSet PackagingFeatures)
isFeatureSupported = HM.fromList [ ("aix", HS.fromList [Installable, Uninstallable, Upgradeable, Versionable])
                                  , ("appdmg", HS.fromList [Installable])
                                  , ("apple", HS.fromList [Installable])
                                  , ("apt", HS.fromList [Holdable, Installable, Purgeable, Uninstallable, Upgradeable, Versionable])
                                  , ("aptitude", HS.fromList [Holdable, Installable, Purgeable, Uninstallable, Upgradeable, Versionable])
                                  , ("aptrpm", HS.fromList [Installable, Purgeable, Uninstallable, Upgradeable, Versionable])
                                  , ("blastwave", HS.fromList [Installable, Uninstallable, Upgradeable])
                                  , ("dpkg", HS.fromList [Holdable, Installable, Purgeable, Uninstallable, Upgradeable])
                                  , ("fink", HS.fromList [Holdable, Installable, Purgeable, Uninstallable, Upgradeable, Versionable])
                                  , ("freebsd", HS.fromList [Installable, Uninstallable])
                                  , ("gem", HS.fromList [Installable, Uninstallable, Upgradeable, Versionable])
                                  , ("hpux", HS.fromList [Installable, Uninstallable])
                                  , ("macports", HS.fromList [Installable, Uninstallable, Upgradeable, Versionable])
                                  , ("msi", HS.fromList [InstallOptions, Installable, UninstallOptions, Uninstallable])
                                  , ("nim", HS.fromList [Installable, Uninstallable, Upgradeable, Versionable])
                                  , ("openbsd", HS.fromList [Installable, Uninstallable, Versionable])
                                  , ("pacman", HS.fromList [Installable, Uninstallable, Upgradeable])
                                  , ("pip", HS.fromList [Installable, Uninstallable, Upgradeable, Versionable])
                                  , ("pkg", HS.fromList [Holdable, Installable, Uninstallable, Upgradeable, Versionable])
                                  , ("pkgdmg", HS.fromList [Installable])
                                  , ("pkgin", HS.fromList [Installable, Uninstallable])
                                  , ("pkgutil", HS.fromList [Installable, Uninstallable, Upgradeable])
                                  , ("portage", HS.fromList [Installable, Uninstallable, Upgradeable, Versionable])
                                  , ("ports", HS.fromList [Installable, Uninstallable, Upgradeable])
                                  , ("portupgrade", HS.fromList [Installable, Uninstallable, Upgradeable])
                                  , ("rpm", HS.fromList [Installable, Uninstallable, Upgradeable, Versionable])
                                  , ("rug", HS.fromList [Installable, Uninstallable, Upgradeable, Versionable])
                                  , ("sun", HS.fromList [InstallOptions, Installable, Uninstallable, Upgradeable])
                                  , ("sunfreeware", HS.fromList [Installable, Uninstallable, Upgradeable])
                                  , ("up2date", HS.fromList [Installable, Uninstallable, Upgradeable])
                                  , ("urpmi", HS.fromList [Installable, Uninstallable, Upgradeable, Versionable])
                                  , ("windows", HS.fromList [InstallOptions, Installable, UninstallOptions, Uninstallable])
                                  , ("yum", HS.fromList [Installable, Purgeable, Uninstallable, Upgradeable, Versionable])
                                  , ("zypper", HS.fromList [Installable, Uninstallable, Upgradeable, Versionable])
                                  ]

parameterfunctions :: [(T.Text, [T.Text -> PuppetTypeValidate])]
parameterfunctions =
    [("adminfile"        , [string, fullyQualified])
    ,("allowcdrom"       , [string, values ["true","false"]])
    ,("configfiles"      , [string, values ["keep","replace"]])
    ,("ensure"           , [defaultvalue "present", string, values ["present","absent","latest","held","purged","installed"]])
    ,("flavor"           , [])
    ,("install_options"  , [rarray])
    ,("name"             , [nameval])
    ,("provider"         , [defaultvalue "apt", string])
    ,("responsefile"     , [string, fullyQualified])
    ,("source"           , [string])
    ,("uninstall_options", [rarray])
    ]

getFeature :: Resource -> Either PrettyError (HS.HashSet PackagingFeatures, Resource)
getFeature res = case res ^. rattributes . at "provider" of
                     Just (PString x) -> case HM.lookup x isFeatureSupported of
                                                    Just s -> Right (s,res)
                                                    Nothing -> Left $ PrettyError ("Do not know provider" <+> ttext x)
                     _ -> Left "Can't happen at Puppet.NativeTypes.Package"

checkFeatures :: (HS.HashSet PackagingFeatures, Resource) -> Either PrettyError Resource
checkFeatures =
        checkAdminFile
        >=> checkEnsure
        >=> checkParam "install_options" InstallOptions
        >=> checkParam "uninstall_options" UninstallOptions
        >=> decap
    where
        checkFeature :: HS.HashSet PackagingFeatures -> Resource -> PackagingFeatures -> Either PrettyError (HS.HashSet PackagingFeatures, Resource)
        checkFeature s r f = if HS.member f s
                                 then Right (s, r)
                                 else Left $ PrettyError ("Feature" <+> text (show f) <+> "is required for the current configuration")
        checkParam :: T.Text -> PackagingFeatures -> (HS.HashSet PackagingFeatures, Resource) -> Either PrettyError (HS.HashSet PackagingFeatures, Resource)
        checkParam pn f (s,r) = if has (ix pn) (r ^. rattributes)
                                    then checkFeature s r f
                                    else Right (s,r)
        checkAdminFile :: (HS.HashSet PackagingFeatures, Resource) -> Either PrettyError (HS.HashSet PackagingFeatures, Resource)
        checkAdminFile = Right -- TODO, check that it only works for aix
        checkEnsure :: (HS.HashSet PackagingFeatures, Resource) -> Either PrettyError (HS.HashSet PackagingFeatures, Resource)
        checkEnsure (s, res) = case res ^. rattributes . at "ensure" of
                                   Just (PString "latest")    -> checkFeature s res Installable >> checkFeature s res Versionable
                                   Just (PString "purged")    -> checkFeature s res Purgeable
                                   Just (PString "absent")    -> checkFeature s res Uninstallable
                                   Just (PString "installed") -> checkFeature s res Installable
                                   Just (PString "present")   -> checkFeature s res Installable
                                   Just (PString "held")      -> checkFeature s res Installable >> checkFeature s res Holdable
                                   _ -> Right (s, res)
        decap :: (HS.HashSet PackagingFeatures, Resource) -> Either PrettyError Resource
        decap = Right . snd
