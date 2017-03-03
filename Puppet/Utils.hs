{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase      #-}
-- | Those are utility functions, most of them being pretty much self
-- explanatory.
module Puppet.Utils (
      textElem
    , getDirectoryContents
    , takeBaseName
    , takeDirectory
    , strictifyEither
    , loadYamlFile
    , scientific2text
    , text2Scientific
    , getFiles
    , ifromList, ikeys, isingleton, ifromListWith, iunionWith, iinsertWith
    -- * re-export
    , module Data.Monoid
) where

import           Data.Attoparsec.Text        (parseOnly, rational)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS
import qualified Data.Foldable               as F
import           Data.Hashable
import qualified Data.HashMap.Strict         as HM
import qualified Data.HashSet                as HS
import Data.Monoid
import System.Posix.Directory.ByteString
import qualified Data.Either.Strict as S
import Data.Scientific
import Control.Exception
import Control.Lens
import Data.Aeson.Lens
import qualified Data.Yaml as Y

text2Scientific :: T.Text -> Maybe Scientific
text2Scientific t = case parseOnly rational t of
            Left _ -> Nothing
            Right s -> Just s

scientific2text :: Scientific -> T.Text
scientific2text n = T.pack $ case n ^? _Integer of
                                 Just i -> show i
                                 _      -> show n

strictifyEither :: Either a b -> S.Either a b
strictifyEither (Left x) = S.Left x
strictifyEither (Right x) = S.Right x

textElem :: Char -> T.Text -> Bool
textElem c = T.any (==c)

getDirectoryContents :: T.Text -> IO [T.Text]
getDirectoryContents fpath = do
    h <- openDirStream (T.encodeUtf8 fpath)
    let readHandle = do
        fp <- readDirStream h
        if BS.null fp
            then return []
            else fmap (T.decodeUtf8 fp :) readHandle
    out <- readHandle
    closeDirStream h
    return out

-- | See System.FilePath.Posix
takeBaseName :: T.Text -> T.Text
takeBaseName fullname =
    let afterLastSlash = last $ T.splitOn "/" fullname
        splitExtension = init $ T.splitOn "." afterLastSlash
    in T.intercalate "." splitExtension

-- | See System.FilePath.Posix
takeDirectory :: T.Text -> T.Text
takeDirectory "" = "."
takeDirectory "/" = "/"
takeDirectory x =
    let res  = T.dropWhileEnd (== '/') file
        file = dropFileName x
    in  if T.null res && not (T.null file)
            then file
            else res

-- | Drop the filename.
--
-- > dropFileName x == fst (splitFileName x)
--
-- (See System.FilePath.Posix)
dropFileName :: T.Text -> T.Text
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
splitFileName :: T.Text -> (T.Text, T.Text)
splitFileName x = (if T.null dir then "./" else dir, name)
    where
        (dir, name) = splitFileName_ x
        splitFileName_ y = (T.reverse b, T.reverse a)
            where
                (a,b) = T.break (=='/') $ T.reverse y

-- | Read a yaml file and throw a runtime error if the parsing fails
loadYamlFile :: Y.FromJSON a =>  FilePath -> IO a
loadYamlFile fp = Y.decodeFileEither fp >>= \case
    Left rr -> error ("Error when parsing " ++ fp ++ ": " ++ show rr)
    Right x -> return x

-- | helper for hashmap, in case we want another kind of map ..
ifromList :: (Monoid m, At m, F.Foldable f) => f (Index m, IxValue m) -> m
{-# INLINABLE ifromList #-}
ifromList = F.foldl' (\curm (k,v) -> curm & at k ?~ v) mempty

ikeys :: (Eq k, Hashable k) => HM.HashMap k v -> HS.HashSet k
{-# INLINABLE ikeys #-}
ikeys = HS.fromList . HM.keys

isingleton :: (Monoid b, At b) => Index b -> IxValue b -> b
{-# INLINABLE isingleton #-}
isingleton k v = mempty & at k ?~ v

ifromListWith :: (Monoid m, At m, F.Foldable f) => (IxValue m -> IxValue m -> IxValue m) -> f (Index m, IxValue m) -> m
{-# INLINABLE ifromListWith #-}
ifromListWith f = F.foldl' (\curmap (k,v) -> iinsertWith f k v curmap) mempty

iinsertWith :: At m => (IxValue m -> IxValue m -> IxValue m) -> Index m -> IxValue m -> m -> m
{-# INLINABLE iinsertWith #-}
iinsertWith f k v m = m & at k %~ mightreplace
    where
        mightreplace Nothing = Just v
        mightreplace (Just x) = Just (f v x)

iunionWith :: (Hashable k, Eq k) => (v -> v -> v) -> HM.HashMap k v -> HM.HashMap k v -> HM.HashMap k v
{-# INLINABLE iunionWith #-}
iunionWith = HM.unionWith

getFiles :: T.Text -> T.Text -> T.Text -> IO [T.Text]
getFiles moduledir subdir extension = fmap concat $
    getDirContents moduledir
        >>= mapM ( checkForSubFiles extension . (\x -> moduledir <> "/" <> x <> "/" <> subdir))

checkForSubFiles :: T.Text -> T.Text -> IO [T.Text]
checkForSubFiles extension dir =
    catch (fmap Right (getDirContents dir)) (\e -> return $ Left (e :: IOException)) >>= \case
        Right o -> return ((map (\x -> dir <> "/" <> x) . filter (T.isSuffixOf extension)) o )
        Left _ -> return []

getDirContents :: T.Text -> IO [T.Text]
getDirContents x = fmap (filter (not . T.all (=='.'))) (getDirectoryContents x)
