{-# LANGUAGE TupleSections #-}

module Puppet.Language.NativeTypes.File (nativeFile) where

import qualified Data.Attoparsec.Text as AT
import qualified Data.Char as Char
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Puppet.Language.NativeTypes.Helpers

nativeFile :: (NativeTypeName, NativeTypeMethods)
nativeFile = ("file", nativetypemethods parameterfunctions (validateSourceOrContent >=> validateMode))

-- Autorequires: If Puppet is managing the user or group that owns a file, the file resource will autorequire them. If Puppet is managing any parent directories of a file, the file resource will autorequire them.

parameterfunctions :: [(Text, [Text -> NativeTypeValidate])]
parameterfunctions =
  [ ("backup", [string]),
    ("checksum", [values ["md5", "md5lite", "mtime", "ctime", "none"]]),
    ("content", [string]),
    -- ,("ensure"               , [defaultvalue "present", string, values ["directory","file","present","absent","link"]])
    ("ensure", [defaultvalue "present", string]),
    ("force", [string, values ["true", "false"]]),
    ("group", [defaultvalue "root", string]),
    ("ignore", [strings]),
    ("links", [string]),
    ("mode", [defaultvalue "0644", string]),
    ("owner", [string]),
    ("path", [nameval, fullyQualified, noTrailingSlash']),
    ("provider", [values ["posix", "windows"]]),
    ("purge", [string, values ["true", "false"]]),
    ("recurse", [string, values ["inf", "true", "false", "remote"]]),
    ("recurselimit", [integer]),
    ("replace", [string, values ["true", "false", "yes", "no"]]),
    ("show_diff", [string, values ["true", "false"]]),
    ("sourceselect", [values ["first", "all"]]),
    ("seltype", [string]),
    ("selrange", [string]),
    ("selinux_ignore_defaults", [string, values ["true", "false"]]),
    ("selrole", [string]),
    ("target", [string]),
    ("source", [rarray, strings, flip runarray checkSource]),
    ("seluser", [string]),
    ("validate_cmd", [string]),
    ("validate_replacement", [string])
  ]

noTrailingSlash' :: Text -> NativeTypeValidate
noTrailingSlash' param res
  | res ^? rattributes . ix "ensure" == Just "directory" = Right res
  | otherwise = noTrailingSlash param res

validateMode :: NativeTypeValidate
validateMode res = do
  modestr <- case res ^. rattributes . at "mode" of
    Just (PString s) -> return s
    Just x -> throwError $ PrettyError ("Invalide mode type, should be a string " <+> pretty x)
    Nothing -> throwError "Could not find mode!"
  (numeric modestr <|> except (ugo modestr)) & runExcept & _Right %~ ($ res)

numeric :: Text -> Except PrettyError (Resource -> Resource)
numeric modestr = do
  when ((Text.length modestr /= 3) && (Text.length modestr /= 4)) (throwError "Invalid mode size")
  unless (Text.all Char.isDigit modestr) (throwError "The mode should only be made of digits")
  return $
    if Text.length modestr == 3
      then rattributes . at "mode" ?~ PString (Text.cons '0' modestr)
      else identity

checkSource :: Text -> PValue -> NativeTypeValidate
checkSource _ (PString x) res
  | any (`Text.isPrefixOf` x) ["puppet://", "file://", "/", "http://", "https://"] = Right res
  | otherwise = throwError "A source should start with either puppet://, http://, https:// or file:// or an absolute path"
checkSource _ x _ = throwError $ PrettyError ("Expected a string, not" <+> pretty x)

data PermParts = Special | User | Group | Other
  deriving (Eq, Ord)

data PermSet = R | W | X
  deriving (Ord, Eq)

ugo :: Text -> Either PrettyError (Resource -> Resource)
ugo t =
  AT.parseOnly (modestring <* AT.endOfInput) t
    & _Left %~ (\rr -> PrettyError $ "Could not parse the mode string: " <> ppstring rr)
    & _Right %~ (\s -> rattributes . at "mode" ?~ PString (mkmode Special s <> mkmode User s <> mkmode Group s <> mkmode Other s))

mkmode :: PermParts -> Map PermParts (Set PermSet) -> Text
mkmode p m =
  let s = m ^. at p . non mempty
   in Text.pack $
        show $
          fromEnum (Set.member R s) * 4
            + fromEnum (Set.member W s) * 2
            + fromEnum (Set.member X s)

modestring :: AT.Parser (Map PermParts (Set.Set PermSet))
modestring = Map.fromList . mconcat <$> (modepart `AT.sepBy` AT.char ',')

-- TODO suid, sticky and other funky things are not yet supported
modepart :: AT.Parser [(PermParts, Set PermSet)]
modepart = do
  let permpart =
        (AT.char 'u' $> [User])
          <|> (AT.char 'g' $> [Group])
          <|> (AT.char 'o' $> [Other])
          <|> (AT.char 'a' $> [User, Group, Other])
      permission =
        (AT.char 'r' $> R)
          <|> (AT.char 'w' $> W)
          <|> (AT.char 'x' $> X)
  pp <- mconcat <$> some permpart
  void $ AT.char '='
  pr <- Set.fromList <$> some permission
  return (map (,pr) pp)
