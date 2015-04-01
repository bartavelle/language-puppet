{-# LANGUAGE LambdaCase      #-}
-- | Those are utility functions, most of them being pretty much self
-- explanatory.
module Puppet.Utils
    ( textElem
    , module Data.Monoid
    , getDirectoryContents
    , takeBaseName
    , takeDirectory
    , strictifyEither
    , nameThread
    , loadYamlFile
    , scientific2text
    ) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS
import Data.Monoid
import System.Posix.Directory.ByteString
import qualified Data.Either.Strict as S
import Control.Concurrent (myThreadId)
import GHC.Conc (labelThread)
import Data.Scientific
import Control.Lens
import Data.Aeson.Lens
import qualified Data.Yaml as Y

scientific2text :: Scientific -> T.Text
scientific2text n = T.pack $ case n ^? _Integer of
                                 Just i -> show i
                                 _      -> show n

strictifyEither :: Either a b -> S.Either a b
strictifyEither (Left x) = S.Left x
strictifyEither (Right x) = S.Right x

textElem :: Char -> T.Text -> Bool
textElem c = T.any (==c)

nameThread :: String -> IO ()
nameThread n = myThreadId >>= flip labelThread n

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
