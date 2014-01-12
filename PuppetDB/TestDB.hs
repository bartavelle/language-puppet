{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, LambdaCase #-}
-- | A stub implementation of PuppetDB, backed by a YAML file.
module PuppetDB.TestDB (loadTestDB,initTestDB) where

import Data.Yaml
import qualified Data.Text as T
import qualified Data.Either.Strict as S
import qualified Data.Vector as V
import Control.Lens
import Control.Exception
import Control.Concurrent.STM
import Data.Monoid
import Control.Applicative
import Data.List (foldl')
import Text.Parsec.Pos
import Data.CaseInsensitive
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS

import Puppet.Parser.Types
import Puppet.Interpreter.Types
import Puppet.Interpreter.Resolve
import Puppet.PP hiding ((<$>))

data DBContent = DBContent { _dbcontentResources   :: Container WireCatalog
                           , _dbcontentFacts       :: Container Facts
                           , _dbcontentBackingFile :: Maybe FilePath
                           }
makeFields ''DBContent

type DB = TVar DBContent

instance FromJSON DBContent where
    parseJSON (Object v) = do
        let toText :: Value -> Parser T.Text
            toText (String m) = pure m
            toText (Number n) = pure (T.pack (show n))
            toText x = fail ("Could not convert " ++ show x ++ " to a string when parsing facts")
        fcts <- v .: "facts" >>= traverse (traverse toText)
        DBContent <$> v .: "resources" <*> pure fcts <*> pure Nothing
    parseJSON _ = mempty

instance ToJSON DBContent where
    toJSON (DBContent r f _) = object [("resources", toJSON r), ("facts", toJSON f)]

-- | Initializes the test DB using a file to back its content
loadTestDB :: FilePath -> IO (S.Either Doc PuppetDBAPI)
loadTestDB fp =
    decodeFileEither fp >>= \case
        Left (OtherParseException rr) -> return (S.Left (string (show rr)))
        Left (InvalidYaml Nothing) -> baseError "Unknown error"
        Left (InvalidYaml (Just (YamlException s))) -> if take 21 s == "Yaml file not found: "
                                                          then newFile
                                                          else baseError (string s)
        Left (InvalidYaml (Just (YamlParseException pb ctx (YamlMark _ l c)))) -> baseError $ red (string pb <+> string ctx) <+> "at line" <+> int l <> ", column" <+> int c
        Left _ -> newFile
        Right x -> fmap S.Right (genDBAPI (x & backingFile ?~ fp ))
    where
        baseError r = return $ S.Left $ "Could not parse" <+> string fp <> ":" <+> r
        newFile = S.Right <$> genDBAPI (newDB & backingFile ?~ fp )

-- | Starts a new PuppetDB, without any backing file.
initTestDB :: IO PuppetDBAPI
initTestDB = genDBAPI newDB

newDB :: DBContent
newDB = DBContent mempty mempty Nothing

genDBAPI :: DBContent -> IO PuppetDBAPI
genDBAPI db = do
    d <- newTVarIO db
    return (PuppetDBAPI (dbapiInfo d)
                        (replCat d)
                        (replFacts d)
                        (deactivate d)
                        (getFcts d)
                        (getRes d)
                        (getNds d)
                        (commit d)
                        (getResNode d)
                        )

data Extracted = EText T.Text
               | ESet (HS.HashSet T.Text)
               | ENil

resolveQuery :: (a -> b -> Extracted) -> Query a -> (b -> Bool)
resolveQuery _ QEmpty = const True
resolveQuery f (QEqual a t) = \v -> case f a v of
                                        EText tt -> mk tt == mk t
                                        ESet ss -> ss ^. contains t
                                        _ -> False
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
        Just v -> return ("TestDB" <+> string v)

ncompare :: (Integer -> Integer -> Bool) ->  (a -> b -> Extracted) -> a -> Integer -> (b -> Bool)
ncompare operation f a i v = case f a v of
                                 EText tt -> case PString tt ^? _PInteger of
                                                 Just ii -> operation i ii
                                                 _ -> False
                                 _ -> False

mustWork :: IO () -> IO (S.Either Doc ())
mustWork a = a >> return (S.Right ())

replCat :: DB -> WireCatalog -> IO (S.Either Doc ())
replCat db wc = mustWork $ atomically $ modifyTVar db (resources . at (wc ^. nodename) ?~ wc)

replFacts :: DB -> [(Nodename, Facts)] -> IO (S.Either Doc ())
replFacts db lst = mustWork $ atomically $ modifyTVar db $
                    facts %~ (\r -> foldl' (\curr (n,f) -> curr & at n ?~ f) r lst)

deactivate :: DB -> Nodename -> IO (S.Either Doc ())
deactivate db n = mustWork $ atomically $ modifyTVar db $
                    (resources . at n .~ Nothing) . (facts . at n .~ Nothing)

getFcts :: DB -> Query FactField -> IO (S.Either Doc [PFactInfo])
getFcts db f = fmap (S.Right . filter (resolveQuery factQuery f) . toFactInfo) (readTVarIO db)
    where
        toFactInfo :: DBContent -> [PFactInfo]
        toFactInfo = concatMap gf .  HM.toList . _dbcontentFacts
            where
                gf (k,n) = do
                    (fn,fv) <- HM.toList n
                    return $ PFactInfo k fn fv
        factQuery :: FactField -> PFactInfo -> Extracted
        factQuery t = EText . view l
            where
                l = case t of
                        FName     -> factname
                        FValue    -> factval
                        FCertname -> nodename

resourceQuery :: ResourceField -> Resource -> Extracted
resourceQuery RTag r = r ^. rtags . to ESet
resourceQuery RCertname r = r ^. rnode . to EText
resourceQuery (RParameter p) r = case r ^? rattributes . ix p . _PString of
                                     Just s -> EText s
                                     Nothing -> ENil
resourceQuery RType r = r ^. rid . itype . to EText
resourceQuery RTitle r = r ^. rid . iname . to EText
resourceQuery RExported r = if r ^. rvirtuality == Exported
                                then EText "true"
                                else EText "false"
resourceQuery RFile r = r ^. rpos . _1 . to sourceName . to T.pack . to EText
resourceQuery RLine r = r ^. rpos . _1 . to sourceLine . to show . to T.pack . to EText

getRes :: DB -> Query ResourceField -> IO (S.Either Doc [Resource])
getRes db f = fmap (S.Right . filter (resolveQuery resourceQuery f) . toResources) (readTVarIO db)
    where
        toResources :: DBContent -> [Resource]
        toResources = concatMap (V.toList . view wResources) .  HM.elems . view resources

getResNode :: DB -> Nodename -> Query ResourceField -> IO (S.Either Doc [Resource])
getResNode db nn f = do
    c <- readTVarIO db
    return $ case c ^. resources . at nn of
                 Just cnt -> S.Right $ filter (resolveQuery resourceQuery f) $ V.toList $ cnt ^. wResources
                 Nothing -> S.Left "Unknown node"

commit :: DB -> IO (S.Either Doc ())
commit db = do
    dbc <- atomically $ readTVar db
    case dbc ^. backingFile of
        Nothing -> return (S.Left "No backing file defined")
        Just bf -> fmap S.Right (encodeFile bf dbc) `catches` [ ]

getNds :: DB -> Query NodeField -> IO (S.Either Doc [PNodeInfo])
getNds _ _ = return (S.Left "getNds not implemented")
