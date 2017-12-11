{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RankNTypes       #-}
-- | General specific prelude for language-puppet
-- | Customization of the Protolude with extra specific utilities
module Puppet.Prelude (
      module Exports
    , String
    , isEmpty
    , dropInitialColons
    , textElem
    , getDirectoryContents
    , takeBaseName
    , takeDirectory
    , strictifyEither
    , scientific2text
    , text2Scientific
    , getFiles
    , loggerName
    , logDebug
    , logInfo
    , logInfoStr
    , logWarning
    , logError
    , logDebugStr
    , ifromList, ikeys, isingleton, ifromListWith, iunionWith, iinsertWith
) where

import           Protolude                         as Exports hiding (Down,
                                                               Infix, Prefix,
                                                               State, StateT,
                                                               Strict, break,
                                                               check, evalState,
                                                               evalStateT,
                                                               execState,
                                                               execStateT, from,
                                                               hash, list,
                                                               moduleName,
                                                               runState,
                                                               runStateT,
                                                               sourceColumn,
                                                               sourceLine, to,
                                                               uncons, unsnoc,
                                                               withState, (%),
                                                               (<&>), (<.>))

import           Control.Exception.Lens            as Exports (catching)
import           Control.Lens                      as Exports hiding (Strict,
                                                               argument, noneOf,
                                                               op)
import           Control.Monad.Trans.Except        as Exports (throwE)
import           Control.Monad.Trans.Maybe         as Exports (runMaybeT)
import           Data.Aeson                        as Exports (fromJSON, toJSON)
import           Data.Scientific                   as Exports (Scientific)
import           Data.Set                          as Exports (Set)
import           Data.Tuple.Strict                 as Exports (Pair (..))
import           Text.Regex.PCRE.ByteString.Utils  as Exports (Regex)

import           Data.Attoparsec.Text              (parseOnly, rational)
import qualified Data.ByteString                   as BS
import qualified Data.Either.Strict                as S
import qualified Data.HashMap.Strict               as HM
import qualified Data.HashSet                      as HS
import qualified Data.List                         as List
import qualified Data.Scientific                   as Scientific
import           Data.String                       (String)
import qualified Data.Text                         as Text
import qualified Data.Text.Encoding                as Text
import qualified System.Log.Logger                 as Log
import           System.Posix.Directory.ByteString

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


-- | See System.FilePath.Posix
takeBaseName :: Text -> Text
takeBaseName fullname =
  let afterLastSlash = List.last $ Text.splitOn "/" fullname
      splitExtension = List.init $ Text.splitOn "." afterLastSlash
  in Text.intercalate "." splitExtension

-- | See System.FilePath.Posix
takeDirectory :: Text -> Text
takeDirectory "" = "."
takeDirectory "/" = "/"
takeDirectory x =
  let res  = Text.dropWhileEnd (== '/') file
      file = dropFileName x
  in  if Text.null res && not (Text.null file)
          then file
          else res

-- | Drop the filename.
--
-- > dropFileName x == fst (splitFileName x)
--
-- (See System.FilePath.Posix)
dropFileName :: Text -> Text
dropFileName = fst . splitFileName

-- | Split a filename into directory and file. 'combine' is the inverse.
--
-- > Valid x => uncurry (</>) (splitFileName x) == x || fst (splitFileName x) == "./"
-- > Valid x => isValid (fst (splitFileName x))
-- > splitFileName "file/bob.txt" == ("file/", "bob.txt")
-- > splitFileName "file/" == ("file/", "")
-- > splitFileName "bob" == ("./", "bob")
-- > Posix:   splitFileName "/" == ("/","")
-- > Windows: splitFileName "c:" == ("c:","")
--
-- (See System.FilePath.Posix)
splitFileName :: Text -> (Text, Text)
splitFileName x =
  (if Text.null dir then "./" else dir, name)
  where
    (dir, name) = splitFileName_ x
    splitFileName_ y =
      let (a,b) = Text.break (=='/') $ Text.reverse y
      in (Text.reverse b, Text.reverse a)

-- | helper for hashmap, in case we want another kind of map ..
ifromList :: (Monoid m, At m, Foldable f) => f (Index m, IxValue m) -> m
{-# INLINABLE ifromList #-}
ifromList = foldl' (\curm (k,v) -> curm & at k ?~ v) mempty

ikeys :: (Eq k, Hashable k) => HM.HashMap k v -> HS.HashSet k
{-# INLINABLE ikeys #-}
ikeys = HS.fromList . HM.keys

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

iunionWith :: (Hashable k, Eq k) => (v -> v -> v) -> HM.HashMap k v -> HM.HashMap k v -> HM.HashMap k v
{-# INLINABLE iunionWith #-}
iunionWith = HM.unionWith

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

-- | remove the '::' token from a text if any
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
