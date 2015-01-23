module Puppet.NativeTypes.File (nativeFile) where

import Puppet.NativeTypes.Helpers hiding ((<$>))
import Control.Monad.Error
import Puppet.Interpreter.Types
import Data.Char (isDigit)
import qualified Data.Text as T
import Control.Lens
import Control.Applicative
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Monoid
import qualified Data.Attoparsec.Text as AT

nativeFile :: (NativeTypeName, NativeTypeMethods)
nativeFile = ("file", nativetypemethods parameterfunctions (validateSourceOrContent >=> validateMode))


-- Autorequires: If Puppet is managing the user or group that owns a file, the file resource will autorequire them. If Puppet is managing any parent directories of a file, the file resource will autorequire them.

parameterfunctions :: [(T.Text, [T.Text -> NativeTypeValidate])]
parameterfunctions =
    [("backup"      , [string])
    ,("checksum"    , [values ["md5", "md5lite", "mtime", "ctime", "none"]])
    ,("content"     , [string])
    --,("ensure"      , [defaultvalue "present", string, values ["directory","file","present","absent","link"]])
    ,("ensure"      , [defaultvalue "present", string])
    ,("force"       , [string, values ["true","false"]])
    ,("group"       , [defaultvalue "root", string])
    ,("ignore"      , [strings])
    ,("links"       , [string])
    ,("mode"        , [defaultvalue "0644", string])
    ,("owner"       , [string])
    ,("path"        , [nameval, fullyQualified, noTrailingSlash])
    ,("provider"    , [values ["posix","windows"]])
    ,("purge"       , [string, values ["true","false"]])
    ,("recurse"     , [string, values ["inf","true","false","remote"]])
    ,("recurselimit", [integer])
    ,("replace"     , [string, values ["true","false"]])
    ,("sourceselect", [values ["first","all"]])
    ,("target"      , [string])
    ,("source"      , [rarray, strings, flip runarray checkSource])
    ]

validateMode :: NativeTypeValidate
validateMode res = do
    modestr <- case res ^. rattributes . at "mode" of
                  Just (PString s) -> return s
                  Just x -> throwError $ PrettyError ("Invalide mode type, should be a string " <+> pretty x)
                  Nothing -> throwError "Could not find mode!"
    (numeric modestr <|> ugo modestr) & _Right %~ ($ res)

numeric :: T.Text -> Either PrettyError (Resource -> Resource)
numeric modestr = do
    when ((T.length modestr /= 3) && (T.length modestr /= 4)) (throwError "Invalid mode size")
    unless (T.all isDigit modestr) (throwError "The mode should only be made of digits")
    return $ if T.length modestr == 3
                 then rattributes . at "mode" ?~ PString (T.cons '0' modestr)
                 else id

checkSource :: T.Text -> PValue -> NativeTypeValidate
checkSource _ (PString x) res | "puppet://" `T.isPrefixOf` x = Right res
                              | "file://" `T.isPrefixOf` x = Right res
                              | otherwise = throwError "A source should start with either puppet:// or file://"
checkSource _ x _ = throwError $ PrettyError ("Expected a string, not" <+> pretty x)

data PermParts = Special | User | Group | Other
               deriving (Eq, Ord)

data PermSet = R | W | X
             deriving (Ord, Eq)

ugo :: T.Text -> Either PrettyError (Resource -> Resource)
ugo t = AT.parseOnly (modestring <* AT.endOfInput) t
            & _Left %~ (\rr -> PrettyError $ "Could not parse the mode string: " <> text rr)
            & _Right %~ (\s -> rattributes . at "mode" ?~ PString (mkmode Special s <> mkmode User s <> mkmode Group s <> mkmode Other s))

mkmode :: PermParts -> M.Map PermParts (S.Set PermSet) -> T.Text
mkmode p m = let s = m ^. at p . non mempty
             in  T.pack $ show $ fromEnum (S.member R s) * 4
                               + fromEnum (S.member W s) * 2
                               + fromEnum (S.member X s)

modestring :: AT.Parser (M.Map PermParts (S.Set PermSet))
modestring = M.fromList . mconcat <$> (modepart `AT.sepBy` AT.char ',')

-- TODO suid, sticky and other funky things are not yet supported
modepart :: AT.Parser [(PermParts, S.Set PermSet)]
modepart = do
    let permpart =   (AT.char 'u' *> pure [User])
                 <|> (AT.char 'g' *> pure [Group])
                 <|> (AT.char 'o' *> pure [Other])
                 <|> (AT.char 'a' *> pure [User,Group,Other])
        permission =   (AT.char 'r' *> pure R)
                   <|> (AT.char 'w' *> pure W)
                   <|> (AT.char 'x' *> pure X)
    pp <- mconcat <$> some permpart
    void $ AT.char '='
    pr <- S.fromList <$> some permission
    return (map (\p -> (p, pr)) pp)
