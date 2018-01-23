{-# OPTIONS_HADDOCK ignore-exports #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
-- | General specific prelude for language-puppet
-- | Customization of the Protolude with extra specific utilities.
module XPrelude.Extra (
      module Exports
    , String
    , Container
    , unwrapError
    , isEmpty
    , dropInitialColons
    , textElem
    , getDirectoryContents
    , takeBaseName
    , strictifyEither
    , scientific2text
    , text2Scientific
    , getFiles
    , ifromList, ikeys, isingleton, ifromListWith, iunionWith, iinsertWith
    -- * Logger
    , loggerName
    , logDebug
    , logInfo
    , logInfoStr
    , logWarning
    , logError
    , logDebugStr
) where

import           Protolude                         as Exports hiding (Down, Infix, Prefix, Selector,
                                                               State, StateT, Strict, break, check,
                                                               evalState, evalStateT, execState,
                                                               execStateT, from, hash, list,
                                                               moduleName, runState, runStateT,
                                                               sourceColumn, sourceLine, to, uncons,
                                                               unsnoc, withState, (%), (<&>), (<.>))

import           Control.Exception.Lens            as Exports (catching)
import           Control.Lens                      as Exports hiding (Strict, argument, noneOf, op)
import           Control.Monad                     as Exports (fail)
import           Control.Monad.Trans.Except        as Exports (except, throwE, catchE)
import           Control.Monad.Trans.Maybe         as Exports (runMaybeT)
import           Data.Aeson                        as Exports (FromJSON, ToJSON, fromJSON, toJSON)
import           Data.HashMap.Strict               as Exports (HashMap)
import           Data.HashSet                      as Exports (HashSet)
import           Data.Scientific                   as Exports (Scientific)
import           Data.Set                          as Exports (Set)
import           Data.String                       as Exports (IsString (..))
import           Data.Tuple.Strict                 as Exports (Pair (..))
import           Data.Vector                       as Exports (Vector)
import           Text.Regex.PCRE.ByteString.Utils  as Exports (Regex)

import           Data.Attoparsec.Text              (parseOnly, rational)
import qualified Data.ByteString                   as BS
import qualified Data.Either.Strict                as S
import qualified Data.HashMap.Strict               as Map
import qualified Data.HashSet                      as HS
import qualified Data.List                         as List
import qualified Data.Scientific                   as Scientific
import           Data.String                       (String)
import qualified Data.Text                         as Text
import qualified Data.Text.Encoding                as Text
import qualified System.Log.Logger                 as Log
import           System.Posix.Directory.ByteString
import           XPrelude.PP

type Container = Map.HashMap Text

text2Scientific :: Text -> Maybe Scientific
text2Scientific t =
  case parseOnly rational t of
    Left _  -> Nothing
    Right s -> Just s

scientific2text :: Scientific -> Text
scientific2text n =
  Text.pack $ case Scientific.floatingOrInteger n of
    Left r  -> show (r :: Double)
    Right i -> show (i :: Integer)

strictifyEither :: Either a b -> S.Either a b
strictifyEither (Left x)  = S.Left x
strictifyEither (Right x) = S.Right x

textElem :: Char -> Text -> Bool
textElem c = Text.any (==c)


-- | See "System.FilePath.Posix"
takeBaseName :: Text -> Text
takeBaseName fullname =
  let afterLastSlash = List.last $ Text.splitOn "/" fullname
      splitExtension = List.init $ Text.splitOn "." afterLastSlash
  in Text.intercalate "." splitExtension

-- | Helper for hashmap, in case we want another kind of map.
ifromList :: (Monoid m, At m, Foldable f) => f (Index m, IxValue m) -> m
{-# INLINABLE ifromList #-}
ifromList = foldl' (\curm (k,v) -> curm & at k ?~ v) mempty

ikeys :: (Eq k, Hashable k) => HashMap k v -> HS.HashSet k
{-# INLINABLE ikeys #-}
ikeys = HS.fromList . Map.keys

isingleton :: (Monoid b, At b) => Index b -> IxValue b -> b
{-# INLINABLE isingleton #-}
isingleton k v = mempty & at k ?~ v

ifromListWith :: (Monoid m, At m, Foldable f) => (IxValue m -> IxValue m -> IxValue m) -> f (Index m, IxValue m) -> m
{-# INLINABLE ifromListWith #-}
ifromListWith f = foldl' (\curmap (k,v) -> iinsertWith f k v curmap) mempty

iinsertWith :: At m => (IxValue m -> IxValue m -> IxValue m) -> Index m -> IxValue m -> m -> m
{-# INLINABLE iinsertWith #-}
iinsertWith f k v m =
  m & at k %~ mightreplace
  where
    mightreplace Nothing  = Just v
    mightreplace (Just x) = Just (f v x)

iunionWith :: (Hashable k, Eq k) => (v -> v -> v) -> HashMap k v -> HashMap k v -> HashMap k v
{-# INLINABLE iunionWith #-}
iunionWith = Map.unionWith

getFiles :: Text -> Text -> Text -> IO [Text]
getFiles moduledir subdir extension =
  fmap concat
  $ getDirContents moduledir
    >>= mapM ( checkForSubFiles extension . (\x -> moduledir <> "/" <> x <> "/" <> subdir))

checkForSubFiles :: Text -> Text -> IO [Text]
checkForSubFiles extension dir =
  catch (fmap Right (getDirContents dir)) (\e -> return $ Left (e :: IOException)) >>= \case
    Right o -> return ((map (\x -> dir <> "/" <> x) . filter (Text.isSuffixOf extension)) o )
    Left _ -> return []

getDirContents :: Text -> IO [Text]
getDirContents x = fmap (filter (not . Text.all (=='.'))) (getDirectoryContents x)

getDirectoryContents :: Text -> IO [Text]
getDirectoryContents fpath = do
  h <- openDirStream (Text.encodeUtf8 fpath)
  let readHandle = do
        fp <- readDirStream h
        if BS.null fp
          then return []
          else fmap (Text.decodeUtf8 fp :) readHandle
  out <- readHandle
  closeDirStream h
  pure out

isEmpty :: (Eq x, Monoid x) => x -> Bool
isEmpty = (== mempty)

-- | Remove the '::' token from a text if any.
dropInitialColons :: Text -> Text
dropInitialColons t = fromMaybe t (Text.stripPrefix "::" t)

loggerName :: String
loggerName = "language-puppet"

logDebug :: Text -> IO ()
logDebug = Log.debugM "language-puppet" . toS

logInfo :: Text -> IO ()
logInfo = Log.infoM "language-puppet" . toS

logInfoStr :: String -> IO ()
logInfoStr = Log.infoM "language-puppet"

logWarning :: Text -> IO ()
logWarning = Log.warningM "language-puppet" . toS

logError :: Text -> IO ()
logError = Log.errorM "language-puppet" . toS

logDebugStr :: String -> IO ()
logDebugStr = Log.debugM "language-puppet"

-- | In case of a Left error, print and exit immediately.
unwrapError :: Doc -> Either PrettyError a -> IO a
unwrapError desc = either exit pure
    where
      exit = \err -> putDoc (display err) >> exitFailure
      display err = red desc <> ":" <+> getError err
