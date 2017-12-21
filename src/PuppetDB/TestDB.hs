{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}

-- | A stub implementation of PuppetDB, backed by a YAML file.
module PuppetDB.TestDB
       ( loadTestDB
       , initTestDB
) where

import           XPrelude

import           Control.Concurrent.STM
import           Data.Aeson
import           Data.Aeson.Lens        (_Integer)
import qualified Data.CaseInsensitive   as CaseInsensitive
import qualified Data.HashMap.Strict    as HM
import qualified Data.HashSet           as HS
import qualified Data.Maybe.Strict      as S
import qualified Data.Text              as Text
import qualified Data.Vector            as V
import           Data.Yaml
import           Text.Megaparsec.Pos

import           Facter
import           Puppet.Language
import           PuppetDB.Core

data DBContent = DBContent
  { _dbcontentResources   :: Container WireCatalog
  , _dbcontentFacts       :: Container Facts
  , _dbcontentBackingFile :: Maybe FilePath
  }

makeLensesWith abbreviatedFields ''DBContent

type DB = TVar DBContent

instance FromJSON DBContent where
  parseJSON (Object v) = DBContent <$> v .: "resources" <*> v .: "facts" <*> pure Nothing
  parseJSON _ = mempty

instance ToJSON DBContent where
  toJSON (DBContent r f _) = object [("resources", toJSON r), ("facts", toJSON f)]

-- | Initializes the test DB using a file to back its content
loadTestDB :: FilePath -> IO (Either PrettyError (PuppetDBAPI IO))
loadTestDB fp =
  decodeFileEither fp >>= \case
    Left (OtherParseException rr) -> return (Left (PrettyError (pplines (show rr))))
    Left (InvalidYaml Nothing) -> baseError "Unknown error"
    Left (InvalidYaml (Just (YamlException s))) -> if take 21 s == "Yaml file not found: "
                                                      then newFile
                                                      else baseError (ppstring s)
    Left (InvalidYaml (Just (YamlParseException pb ctx (YamlMark _ l c)))) -> baseError $ red (ppstring pb <+> ppstring ctx) <+> "at line" <+> pretty l <> ", column" <+> pretty c
    Left _ -> newFile
    Right x -> fmap Right (genDBAPI (x & backingFile ?~ fp ))
  where
    baseError r = return $ Left $ PrettyError $ "Could not parse" <+> pptext fp <> ":" <+> r
    newFile = Right <$> genDBAPI (newDB & backingFile ?~ fp )

-- | Starts a new PuppetDB, without any backing file.
initTestDB :: IO (PuppetDBAPI IO)
initTestDB = genDBAPI newDB

newDB :: DBContent
newDB = DBContent mempty mempty Nothing

genDBAPI :: DBContent -> IO (PuppetDBAPI IO)
genDBAPI db = do
  d <- newTVarIO db
  return $! PuppetDBAPI (dbapiInfo d)
                        (replCat d)
                        (replFacts d)
                        (deactivate d)
                        (getFcts d)
                        (getRes d)
                        (getNds d)
                        (commit d)
                        (getResNode d)


data Extracted
  = EText Text
  | ESet (HS.HashSet Text)
  | ENil

resolveQuery :: (a -> b -> Extracted) -> Query a -> b -> Bool
resolveQuery _ QEmpty = const True
resolveQuery f (QEqual a t) =
  \v -> case f a v of
    EText tt -> CaseInsensitive.mk tt == CaseInsensitive.mk t
    ESet ss  -> ss ^. contains t
    _        -> False
resolveQuery f (QNot q)  = not . resolveQuery f q
resolveQuery f (QG a i)  = ncompare (>) f a i
resolveQuery f (QL a i)  = ncompare (<) f a i
resolveQuery f (QGE a i) = ncompare (>=) f a i
resolveQuery f (QLE a i) = ncompare (<=) f a i
resolveQuery _ (QMatch _ _) = const False
resolveQuery f (QAnd qs) = \v -> all (\q -> resolveQuery f q v) qs
resolveQuery f (QOr qs)  = \v -> any (\q -> resolveQuery f q v) qs

dbapiInfo :: DB -> IO Doc
dbapiInfo db = do
  c <- readTVarIO db
  case c ^. backingFile of
    Nothing -> return "TestDB"
    Just v  -> return ("TestDB" <+> ppstring v)

ncompare :: (Integer -> Integer -> Bool) ->  (a -> b -> Extracted) -> a -> Integer -> b -> Bool
ncompare operation f a i v =
  case f a v of
    EText tt -> case PString tt ^? _Integer of
                    Just ii -> operation i ii
                    _       -> False
    _ -> False

replCat :: DB -> WireCatalog -> ExceptT PrettyError IO ()
replCat db wc = liftIO $ atomically $ modifyTVar db (resources . at (wc ^. wireCatalogNodename) ?~ wc)

replFacts :: DB -> [(NodeName, Facts)] -> ExceptT PrettyError IO ()
replFacts db lst = liftIO $ atomically $ modifyTVar db $
                    facts %~ (\r -> foldl' (\curr (n,f) -> curr & at n ?~ f) r lst)

deactivate :: DB -> NodeName -> ExceptT PrettyError IO ()
deactivate db n = liftIO $ atomically $ modifyTVar db $
                    (resources . at n .~ Nothing) . (facts . at n .~ Nothing)

getFcts :: DB -> Query FactField -> ExceptT PrettyError IO [FactInfo]
getFcts db f = fmap (filter (resolveQuery factQuery f) . toFactInfo) (liftIO $ readTVarIO db)
    where
        toFactInfo :: DBContent -> [FactInfo]
        toFactInfo = concatMap gf .  HM.toList . _dbcontentFacts
            where
                gf (k,n) = do
                    (fn,fv) <- HM.toList n
                    return $ FactInfo k fn fv
        factQuery :: FactField -> FactInfo -> Extracted
        factQuery t = EText . view l
            where
                l = case t of
                        FName     -> factInfoName
                        FValue    -> factInfoVal . _PString
                        FCertname -> factInfoNodename

resourceQuery :: ResourceField -> Resource -> Extracted
resourceQuery RTag r = r ^. rtags . to ESet
resourceQuery RCertname r = r ^. rnode . to EText
resourceQuery (RParameter p) r = case r ^? rattributes . ix p . _PString of
                                     Just s  -> EText s
                                     Nothing -> ENil
resourceQuery RType r = r ^. rid . itype . to EText
resourceQuery RTitle r = r ^. rid . iname . to EText
resourceQuery RExported r = if r ^. rvirtuality == Exported
                                then EText "true"
                                else EText "false"
resourceQuery RFile r = r ^. rpos . _1 . to sourceName . to Text.pack . to EText
resourceQuery RLine r = r ^. rpos . _1 . to sourceLine . to show . to Text.pack . to EText

getRes :: DB -> Query ResourceField -> ExceptT PrettyError IO [Resource]
getRes db f = fmap (filter (resolveQuery resourceQuery f) . toResources) (liftIO $ readTVarIO db)
    where
        toResources :: DBContent -> [Resource]
        toResources = concatMap (V.toList . view wireCatalogResources) .  HM.elems . view resources

getResNode :: DB -> NodeName -> Query ResourceField -> ExceptT PrettyError IO [Resource]
getResNode db nn f = do
    c <- liftIO $ readTVarIO db
    case c ^. resources . at nn of
        Just cnt -> return $ filter (resolveQuery resourceQuery f) $ V.toList $ cnt ^. wireCatalogResources
        Nothing -> throwError "Unknown node"

commit :: DB -> ExceptT PrettyError IO ()
commit db = do
    dbc <- liftIO $ atomically $ readTVar db
    case dbc ^. backingFile of
        Nothing -> throwError "No backing file defined"
        Just bf -> liftIO (encodeFile bf dbc `catches` [ ])

getNds :: DB -> Query NodeField -> ExceptT PrettyError IO [NodeInfo]
getNds db QEmpty = fmap toNodeInfo (liftIO $ readTVarIO db)
    where
        toNodeInfo :: DBContent -> [NodeInfo]
        toNodeInfo = fmap g . HM.keys . _dbcontentFacts
             where
                g :: NodeName -> NodeInfo
                g = \n -> NodeInfo n False S.Nothing S.Nothing S.Nothing

getNds _ _ = throwError "getNds with query not implemented"
