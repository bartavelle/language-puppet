module Puppet.NativeTypes.Package (nativePackage) where

import Puppet.NativeTypes.Helpers
import Puppet.Interpreter.Types
import Control.Monad.Error
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Text as T

nativePackage :: (PuppetTypeName, PuppetTypeMethods)
nativePackage = ("package", PuppetTypeMethods validatePackage parameterset)

data PackagingFeatures = Holdable | InstallOptions | Installable | Purgeable | UninstallOptions | Uninstallable | Upgradeable | Versionable deriving (Show, Ord, Eq)

isFeatureSupported :: Map.Map T.Text (Set.Set PackagingFeatures)
isFeatureSupported = Map.fromList [ ("aix", Set.fromList [Installable, Uninstallable, Upgradeable, Versionable])
                                  , ("appdmg", Set.fromList [Installable])
                                  , ("apple", Set.fromList [Installable])
                                  , ("apt", Set.fromList [Holdable, Installable, Purgeable, Uninstallable, Upgradeable, Versionable])
                                  , ("aptitude", Set.fromList [Holdable, Installable, Purgeable, Uninstallable, Upgradeable, Versionable])
                                  , ("aptrpm", Set.fromList [Installable, Purgeable, Uninstallable, Upgradeable, Versionable])
                                  , ("blastwave", Set.fromList [Installable, Uninstallable, Upgradeable])
                                  , ("dpkg", Set.fromList [Holdable, Installable, Purgeable, Uninstallable, Upgradeable])
                                  , ("fink", Set.fromList [Holdable, Installable, Purgeable, Uninstallable, Upgradeable, Versionable])
                                  , ("freebsd", Set.fromList [Installable, Uninstallable])
                                  , ("gem", Set.fromList [Installable, Uninstallable, Upgradeable, Versionable])
                                  , ("hpux", Set.fromList [Installable, Uninstallable])
                                  , ("macports", Set.fromList [Installable, Uninstallable, Upgradeable, Versionable])
                                  , ("msi", Set.fromList [InstallOptions, Installable, UninstallOptions, Uninstallable])
                                  , ("nim", Set.fromList [Installable, Uninstallable, Upgradeable, Versionable])
                                  , ("openbsd", Set.fromList [Installable, Uninstallable, Versionable])
                                  , ("pacman", Set.fromList [Installable, Uninstallable, Upgradeable])
                                  , ("pip", Set.fromList [Installable, Uninstallable, Upgradeable, Versionable])
                                  , ("pkg", Set.fromList [Holdable, Installable, Uninstallable, Upgradeable, Versionable])
                                  , ("pkgdmg", Set.fromList [Installable])
                                  , ("pkgin", Set.fromList [Installable, Uninstallable])
                                  , ("pkgutil", Set.fromList [Installable, Uninstallable, Upgradeable])
                                  , ("portage", Set.fromList [Installable, Uninstallable, Upgradeable, Versionable])
                                  , ("ports", Set.fromList [Installable, Uninstallable, Upgradeable])
                                  , ("portupgrade", Set.fromList [Installable, Uninstallable, Upgradeable])
                                  , ("rpm", Set.fromList [Installable, Uninstallable, Upgradeable, Versionable])
                                  , ("rug", Set.fromList [Installable, Uninstallable, Upgradeable, Versionable])
                                  , ("sun", Set.fromList [InstallOptions, Installable, Uninstallable, Upgradeable])
                                  , ("sunfreeware", Set.fromList [Installable, Uninstallable, Upgradeable])
                                  , ("up2date", Set.fromList [Installable, Uninstallable, Upgradeable])
                                  , ("urpmi", Set.fromList [Installable, Uninstallable, Upgradeable, Versionable])
                                  , ("windows", Set.fromList [InstallOptions, Installable, UninstallOptions, Uninstallable])
                                  , ("yum", Set.fromList [Installable, Purgeable, Uninstallable, Upgradeable, Versionable])
                                  , ("zypper", Set.fromList [Installable, Uninstallable, Upgradeable, Versionable])
                                  ]

parameterset = Set.fromList $ map fst parameterfunctions
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

getFeature :: RResource -> Either String (Set.Set PackagingFeatures, RResource)
getFeature res = case Map.lookup "provider" (rrparams res) of
                     Just (ResolvedString x) -> case Map.lookup x isFeatureSupported of
                                                    Just s -> Right (s,res)
                                                    Nothing -> Left ("Do not know provider " ++ T.unpack x)
                     _ -> Left "Can't happen at Puppet.NativeTypes.Package"

checkFeatures :: (Set.Set PackagingFeatures, RResource) -> Either String RResource
checkFeatures =
        checkAdminFile
        >=> checkEnsure
        >=> checkParam "install_options" InstallOptions
        >=> checkParam "uninstall_options" UninstallOptions
        >=> decap
    where
        checkFeature :: Set.Set PackagingFeatures -> RResource -> PackagingFeatures -> Either String (Set.Set PackagingFeatures, RResource)
        checkFeature s r f = if Set.member f s
                                 then Right (s, r)
                                 else Left ("Feature " ++ show f ++ " is required for the current configuration")
        checkParam :: T.Text -> PackagingFeatures -> (Set.Set PackagingFeatures, RResource) -> Either String (Set.Set PackagingFeatures, RResource)
        checkParam pn f (s,r) = if Map.member pn (rrparams r)
                                    then checkFeature s r f
                                    else Right (s,r)
        checkAdminFile :: (Set.Set PackagingFeatures, RResource) -> Either String (Set.Set PackagingFeatures, RResource)
        checkAdminFile = Right -- TODO, check that it only works for aix
        checkEnsure :: (Set.Set PackagingFeatures, RResource) -> Either String (Set.Set PackagingFeatures, RResource)
        checkEnsure (s, res) = case Map.lookup "ensure" (rrparams res) of
                                   Just (ResolvedString "latest")    -> checkFeature s res Installable >> checkFeature s res Versionable
                                   Just (ResolvedString "purged")    -> checkFeature s res Purgeable
                                   Just (ResolvedString "absent")    -> checkFeature s res Uninstallable
                                   Just (ResolvedString "installed") -> checkFeature s res Installable
                                   Just (ResolvedString "present")   -> checkFeature s res Installable
                                   Just (ResolvedString "held")      -> checkFeature s res Installable >> checkFeature s res Holdable
                                   _ -> Right (s, res)
        decap :: (Set.Set PackagingFeatures, RResource) -> Either String RResource
        decap = Right . snd

validatePackage :: PuppetTypeValidate
validatePackage = defaultValidate parameterset >=> parameterFunctions parameterfunctions >=> getFeature >=> checkFeatures


