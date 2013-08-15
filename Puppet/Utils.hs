module Puppet.Utils
    ( readDecimal
    , readRational
    , puppet2number
    , textElem
    , module Data.Monoid
    , getDirectoryContents
    , takeBaseName
    , takeDirectory
    , strictifyEither
    ) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Read as T
import qualified Data.ByteString as BS
import Data.Monoid
import System.Posix.Directory.ByteString
import qualified Data.Either.Strict as S

import Puppet.Interpreter.Types

strictifyEither :: Either a b -> S.Either a b
strictifyEither (Left x) = S.Left x
strictifyEither (Right x) = S.Right x

readDecimal :: (Integral a) => T.Text -> Either String a
readDecimal t = case T.signed T.decimal t of
                    Right (x, "") -> Right x
                    Right _ -> Left "Trailing characters when reading an integer"
                    Left r -> Left r

readRational :: Fractional a => T.Text -> Either String a
readRational t = case T.signed T.rational t of
                    Right (x, "") -> Right x
                    Right _ -> Left "Trailing characters when reading an integer"
                    Left r -> Left r

puppet2number :: PValue -> Maybe (Either Double Integer)
puppet2number (PString s) = case (readDecimal s, readRational s) of
                                (Right i,_) -> Just (Right i)
                                (_,Right d) -> Just (Left d)
                                _ -> Nothing
puppet2number _ = Nothing

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

