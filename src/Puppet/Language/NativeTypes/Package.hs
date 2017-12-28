{-# LANGUAGE DeriveGeneric #-}
module Puppet.Language.NativeTypes.Package
  ( nativePackage
  ) where

import qualified Data.HashMap.Strict                 as HM
import qualified Data.HashSet                        as Set

import           Puppet.Language.NativeTypes.Helpers

nativePackage :: (NativeTypeName, NativeTypeMethods)
nativePackage = ("package", nativetypemethods parameterfunctions (getFeature >=> checkFeatures))

-- Features are abilities that some providers may not support.
data PackagingFeatures
  = Holdable
  | InstallOptions
  | Installable
  | Purgeable
  | UninstallOptions
  | Uninstallable
  | Upgradeable
  | Versionable
  deriving (Show, Eq, Generic)

instance Pretty PackagingFeatures where
  pretty = ppline . show

instance Hashable PackagingFeatures

isFeatureSupported :: HashMap Text (HashSet PackagingFeatures)
isFeatureSupported = HM.fromList [ ("aix", Set.fromList [Installable, Uninstallable, Upgradeable, Versionable])
                                  , ("appdmg", Set.fromList [Installable])
                                  , ("apple", Set.fromList [Installable])
                                  , ("apt", Set.fromList [Holdable, InstallOptions, Installable, Purgeable, Uninstallable, Upgradeable, Versionable])
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

parameterfunctions :: [(Text, [Text -> NativeTypeValidate])]
parameterfunctions =
  [("adminfile"        , [string, fullyQualified])
  ,("allowcdrom"       , [string, values ["true","false"]])
  ,("configfiles"      , [string, values ["keep","replace"]])
  --,("ensure"           , [defaultvalue "present", string, values ["present","absent","latest","held","purged","installed"]])
  ,("ensure"           , [defaultvalue "present", string])
  ,("flavor"           , [])
  ,("install_options"  , [rarray])
  ,("name"             , [nameval])
  ,("provider"         , [defaultvalue "apt", string])
  ,("responsefile"     , [string, fullyQualified])
  ,("source"           , [string])
  ,("uninstall_options", [rarray])
  ]

getFeature :: Resource -> Either PrettyError (HashSet PackagingFeatures, Resource)
getFeature res =
  case res ^. rattributes . at "provider" of
    Just (PString x) ->
      case HM.lookup x isFeatureSupported of
        Just s  -> Right (s, res)
        Nothing -> Left $ PrettyError ("Do not know provider" <+> ppline x)
    _ -> Left "Can't happen at Puppet.NativeTypes.Package"

checkFeatures :: (HashSet PackagingFeatures, Resource) -> Either PrettyError Resource
checkFeatures =
  checkAdminFile
  >=> checkEnsure
  >=> checkParam "install_options" InstallOptions
  >=> checkParam "uninstall_options" UninstallOptions
  >=> decap
  where
      checkFeature :: HashSet PackagingFeatures -> Resource -> PackagingFeatures -> Either PrettyError (HashSet PackagingFeatures, Resource)
      checkFeature s r f = if Set.member f s
                             then Right (s, r)
                             else Left $ PrettyError ("Feature" <+> pretty f <+> "is required for the current configuration")
      checkParam :: Text -> PackagingFeatures -> (HashSet PackagingFeatures, Resource) -> Either PrettyError (HashSet PackagingFeatures, Resource)
      checkParam pn f (s,r) = if has (ix pn) (r ^. rattributes)
                                  then checkFeature s r f
                                  else Right (s,r)
      checkAdminFile :: (HashSet PackagingFeatures, Resource) -> Either PrettyError (HashSet PackagingFeatures, Resource)
      checkAdminFile = Right -- TODO, check that it only works for aix
      checkEnsure :: (HashSet PackagingFeatures, Resource) -> Either PrettyError (HashSet PackagingFeatures, Resource)
      checkEnsure (s, res) = case res ^. rattributes . at "ensure" of
                                 Just (PString "latest")    -> checkFeature s res Installable
                                 Just (PString "purged")    -> checkFeature s res Purgeable
                                 Just (PString "absent")    -> checkFeature s res Uninstallable
                                 Just (PString "installed") -> checkFeature s res Installable
                                 Just (PString "present")   -> checkFeature s res Installable
                                 Just (PString "held")      -> checkFeature s res Installable >> checkFeature s res Holdable
                                 _ -> checkFeature s res Versionable
      decap :: (HashSet PackagingFeatures, Resource) -> Either PrettyError Resource
      decap = Right . snd
