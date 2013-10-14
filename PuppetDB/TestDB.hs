{-# LANGUAGE TemplateHaskell #-}
module PuppetDB.TestDB (loadTestDB,initTestDB) where

import Data.Aeson
import qualified Data.Text as T
import qualified Data.Either.Strict as S
import qualified Data.Vector as V
import Control.Lens
import Control.Exception
import Control.Exception.Lens
import System.IO.Error.Lens
import Control.Concurrent.STM
import Data.Monoid
import Control.Applicative
import qualified Data.ByteString.Lazy as BS

import Puppet.Interpreter.Types
import Puppet.PP hiding ((<$>))

data DBContent = DBContent { _resources   :: Container [Resource]
                           , _facts       :: Container Facts
                           , _backingFile :: Maybe FilePath
                           }
makeClassy ''DBContent

type DB = TVar DBContent

instance FromJSON DBContent where
    parseJSON (Object v) = DBContent <$> v .: "resources" <*> v .: "facts" <*> pure Nothing
    parseJSON _ = mempty

-- | Initializes the test DB using a file to back its content
loadTestDB :: FilePath -> IO (S.Either Doc PuppetDBAPI)
loadTestDB fp = do
    fcontent <- fmap eitherDecode' (BS.readFile fp) `catches` [ handler_ id (return (Right newDB))
                                                              , handler id (return . Left . show)
                                                              ]
    case fcontent of
        Left rr -> return (S.Left (string rr))
        Right x -> fmap S.Right (genDBAPI (x & backingFile ?~ fp ))


initTestDB :: IO PuppetDBAPI
initTestDB = genDBAPI newDB

newDB :: DBContent
newDB = DBContent mempty mempty Nothing

genDBAPI :: DBContent -> IO PuppetDBAPI
genDBAPI db = do
    d <- newTVarIO db
    return (PuppetDBAPI (replCat d)
                        (replFacts d)
                        (deactivate d)
                        (getFcts d)
                        (getRes d)
                        (commit d))

replCat :: DB -> FinalCatalog -> IO (S.Either Doc ())
replCat = undefined

replFacts :: DB -> [(Nodename, Facts)] -> IO (S.Either Doc ())
replFacts = undefined

deactivate :: DB -> Nodename -> IO (S.Either Doc ())
deactivate = undefined

getFcts :: DB -> Query FactField -> IO (S.Either Doc [(Nodename, T.Text, PValue)])
getFcts = undefined

getRes :: DB -> Query ResourceField -> IO (S.Either Doc [Resource])
getRes = undefined

commit :: DB -> IO (S.Either Doc ())
commit = undefined
