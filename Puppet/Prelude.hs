{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
-- | General specific prelude for language-puppet
-- | Customization of the Protolude with extra specific utilities
module Puppet.Prelude (
      module Exports
    , String
    , textElem
    , getDirectoryContents
    , takeBaseName
    , takeDirectory
    , strictifyEither
    , scientific2text
    , text2Scientific
    , getFiles
    , ifromList, ikeys, isingleton, ifromListWith, iunionWith, iinsertWith
) where

import           Protolude                         as Exports hiding (Down, Infix, Prefix,
                                                                      State, StateT, Strict, break,
                                                                      check, evalState,
                                                                      runState, runStateT, evalStateT, execState, execStateT, withState,
                                                                      from, hash, list, to,
                                                                      moduleName, sourceColumn, sourceLine,
                                                                      uncons, unsnoc,
                                                                      (%), (<&>), (<.>))

import           Control.Lens                      as Exports hiding (Strict,
                                                               noneOf, op, argument)
import           Data.Scientific                   as Exports (Scientific)
import           Control.Monad.Trans.Maybe         as Exports (runMaybeT)
import           Data.Tuple.Strict                 as Exports (Pair(..))
import           Control.Monad.Trans.Except        as Exports (throwE)
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
import           System.Posix.Directory.ByteString

text2Scientific :: Text -> Maybe Scientific
text2Scientific t = case parseOnly rational t of
            Left _  -> Nothing
            Right s -> Just s

scientific2text :: Scientific -> Text
scientific2text n = Text.pack $ case Scientific.floatingOrInteger n of
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
splitFileName x = (if Text.null dir then "./" else dir, name)
    where
        (dir, name) = splitFileName_ x
        splitFileName_ y = (Text.reverse b, Text.reverse a)
            where
                (a,b) = Text.break (=='/') $ Text.reverse y

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
iinsertWith f k v m = m & at k %~ mightreplace
    where
        mightreplace Nothing  = Just v
        mightreplace (Just x) = Just (f v x)

iunionWith :: (Hashable k, Eq k) => (v -> v -> v) -> HM.HashMap k v -> HM.HashMap k v -> HM.HashMap k v
{-# INLINABLE iunionWith #-}
iunionWith = HM.unionWith

getFiles :: Text -> Text -> Text -> IO [Text]
getFiles moduledir subdir extension = fmap concat $
    getDirContents moduledir
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
