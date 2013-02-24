{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module Puppet.Utils
    ( mGetExecutablePath
    , readFile'
    , readSymbolicLink
    , tshow
    , dq
    , readDecimal
    , textElem
    , module Data.Monoid
    , getDirectoryContents
    , takeBaseName
    , takeDirectory
    , regexpSplit
    , regexpMatched
    , regexpUnmatched
    , regexpAll
    , RegexpSplit(..)
    ) where

-- copy pasted from base 4.6.0.0
import Prelude hiding (catch)
import Foreign.C
import Foreign.Marshal.Array
import System.Posix.Internals
import System.IO
import Control.Exception
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Read as T
import qualified Data.ByteString as BS
import Data.Monoid
import System.Posix.Directory.ByteString
import Text.Regex.PCRE.ByteString
import Control.Monad.Error

foreign import ccall unsafe "readlink" c_readlink :: CString -> CString -> CSize -> IO CInt

readSymbolicLink :: FilePath -> IO FilePath
readSymbolicLink file =
    allocaArray0 4096 $ \buf -> do
        withFilePath file $ \s -> do
            len <- throwErrnoPathIfMinus1 "readSymbolicLink" file $
                   c_readlink s buf 4096
            peekFilePathLen (buf,fromIntegral len)

-- | Returns the absolute pathname of the current executable.
--
-- Note that for scripts and interactive sessions, this is the path to
-- the interpreter (e.g. ghci.)
-- (Stolen from base 4.6.0)
mGetExecutablePath :: IO FilePath
mGetExecutablePath = readSymbolicLink $ "/proc/self/exe"

-- | Strict readFile
readFile' f = do
    h <- openFile f ReadMode
    s <- hGetContents h
    evaluate (length s)
    return s

tshow :: Show a => a -> T.Text
tshow = T.pack . show

dq :: T.Text -> T.Text
dq x = T.cons '"' (T.snoc x '"')

readDecimal :: (Integral a) => T.Text -> Either String a
readDecimal t = case T.decimal t of
                    Right (x, "") -> Right x
                    Right _ -> Left "Trailing characters when reading an integer"
                    Left r -> Left r

textElem :: Char -> T.Text -> Bool
textElem c t = T.any (==c) t

getDirectoryContents :: T.Text -> IO [T.Text]
getDirectoryContents fpath = do
    h <- openDirStream (T.encodeUtf8 fpath)
    let readHandle = do
        fp <- readDirStream h
        if BS.null fp
            then return []
            else fmap (\e -> T.decodeUtf8 fp : e) readHandle
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
    in  if T.null res && (not (T.null file))
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

data RegexpSplit a = Matched a
                   | Unmatched a
                   deriving (Show, Eq, Ord)

instance Functor RegexpSplit where
    fmap f (Matched x)   = Matched (f x)
    fmap f (Unmatched x) = Unmatched (f x)

regexpAll :: [RegexpSplit a] -> [a]
regexpAll = map unreg
    where
        unreg ( Matched x   ) = x
        unreg ( Unmatched x ) = x

isMatched :: RegexpSplit a -> Bool
isMatched (Matched _) = True
isMatched _ = False

regexpMatched :: [RegexpSplit a] -> [a]
regexpMatched = regexpAll . filter isMatched

regexpUnmatched :: [RegexpSplit a] -> [a]
regexpUnmatched = regexpAll . filter (not . isMatched)

regexpSplit :: CompOption -> T.Text -> T.Text -> IO (Either String [RegexpSplit T.Text])
regexpSplit opt reg src = runErrorT $ do
    creg <- liftIO $ compile opt execBlank (T.encodeUtf8 reg)
        >>= \x -> case x of
                      Right r -> return r
                      Left rr -> error (show rr)
    fmap (map (fmap T.decodeUtf8)) $ getMatches opt creg (T.encodeUtf8 src)

getMatches :: CompOption -> Regex -> BS.ByteString -> ErrorT String IO [RegexpSplit BS.ByteString]
getMatches _ _ ""  = return []
getMatches opt creg src = do
    x <- liftIO (regexec creg src)
    case x of
        Right Nothing -> return [Unmatched src]
        Right (Just (before,current,remaining,_)) -> do
            remain <- getMatches opt creg remaining
            if BS.null before
                then return (Matched current : remain)
                else return (Unmatched before : Matched current : remain)
        Left (rcode, rerror) -> throwError ("Regexp application error: " ++ rerror ++ "(" ++ show rcode ++ ")")
