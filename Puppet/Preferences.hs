{-# LANGUAGE TemplateHaskell #-}
module Puppet.Preferences where

import Puppet.Utils
import Puppet.Interpreter.Types
import Puppet.Plugins
import Puppet.NativeTypes
import Puppet.NativeTypes.Helpers
import Puppet.Stdlib
import PuppetDB.TestDB

import qualified Data.Either.Strict as S
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Aeson
import qualified Data.HashMap.Strict as HM
import Control.Lens

data Preferences = Preferences
    { _manifestPath    :: FilePath -- ^ The path to the manifests.
    , _modulesPath     :: FilePath -- ^ The path to the modules.
    , _templatesPath   :: FilePath -- ^ The path to the template.
    , _compilePoolSize :: Int -- ^ Size of the compiler pool.
    , _parsePoolSize   :: Int -- ^ Size of the parser pool.
    , _pDBquery        :: T.Text -> Value -> IO (S.Either String Value) -- ^ A function that takes a query type, a query and might return stuff
    , _natTypes        :: Container PuppetTypeMethods -- ^ The list of native types.
    , _prefExtFuncs    :: Container ( [PValue] -> InterpreterMonad PValue )
    }

makeClassy ''Preferences

genPreferences :: Bool              -- ^ will initialize the dummy puppetdb when True
               -> FilePath
               -> IO Preferences
genPreferences testpdb basedir = do
    let manifestdir = basedir <> "/manifests"
        modulesdir  = basedir <> "/modules"
        templatedir = basedir <> "/templates"
    typenames <- fmap (map takeBaseName) (getFiles (T.pack modulesdir) "lib/puppet/type" ".rb")
    let loadedTypes = HM.fromList (map defaulttype typenames)
        cstpdb :: T.Text -> Value -> IO (S.Either String Value)
        cstpdb _ _ = return (S.Right (Array V.empty))
    selectedpdb <- if testpdb
                       then initPuppetDB
                       else return cstpdb
    return $ Preferences manifestdir modulesdir templatedir 4 4 selectedpdb (baseNativeTypes `HM.union` loadedTypes) (stdlibFunctions)
