{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
--  Private modules.
--  Function and data types that are used to define the native types.
module Puppet.Language.NativeTypes.Helpers
  ( module Exports
  , ipaddr
  , nativetypemethods
  , NativeTypeName
  , NativeTypeValidate
  , NativeTypeMethods
  , HasNativeTypeMethods(..)
  , paramname
  , rarray
  , string
  , strings
  , string_s
  , noTrailingSlash
  , fullyQualified
  , fullyQualifieds
  , values
  , defaultvalue
  , nameval
  , defaultValidate
  , integer
  , integers
  , mandatory
  , mandatoryIfNotAbsent
  , inrange
  , faketype
  , defaulttype
  , runarray
  , perror
  , validateSourceOrContent
  ) where

import           XPrelude                 as Exports

import           Data.Char                as Char
import qualified Data.HashMap.Strict      as Map
import qualified Data.HashSet             as HS
import qualified Data.Text                as Text
import qualified Data.Vector              as V

import           Puppet.Language.Core     as Exports
import           Puppet.Language.Resource as Exports
import           Puppet.Language.Value    as Exports
import qualified Text.Read


metaparameters :: HS.HashSet Text
metaparameters = HS.fromList ["tag","stage","name","title","alias","audit","check","loglevel","noop","schedule", "EXPORTEDSOURCE", "require", "before", "register", "notify"]

type NativeTypeName = Text
type NativeTypeValidate = Resource -> Either PrettyError Resource

-- | Attributes (and providers) of a puppet resource type bundled with validation rules
data NativeTypeMethods = NativeTypeMethods
  { _puppetValidate :: NativeTypeValidate
  , _puppetFields   :: HS.HashSet Text
  }

makeClassy ''NativeTypeMethods

paramname :: Text -> Doc
paramname = red . ppline

-- | Useful helper for buiding error messages
perror :: Doc -> Either PrettyError Resource
perror = Left . PrettyError

-- | Smart constructor for 'NativeTypeMethods'.
nativetypemethods :: [(Text, [Text -> NativeTypeValidate])] -> NativeTypeValidate -> NativeTypeMethods
nativetypemethods def extraV =
  let params = fromKeys def
  in NativeTypeMethods (defaultValidate params >=> parameterFunctions def >=> extraV)  params
  where
    fromKeys = HS.fromList . map fst

faketype :: NativeTypeName -> (NativeTypeName, NativeTypeMethods)
faketype tname = (tname, NativeTypeMethods Right HS.empty)

defaulttype :: NativeTypeName -> (NativeTypeName, NativeTypeMethods)
defaulttype tname = (tname, NativeTypeMethods (defaultValidate HS.empty) HS.empty)

{-| Validate resources given a list of valid parameters:

      * checks that no unknown parameters have been set (except metaparameters)
-}
defaultValidate :: HS.HashSet Text -> NativeTypeValidate
defaultValidate validparameters = checkParameterList validparameters >=> addDefaults

checkParameterList :: HS.HashSet Text -> NativeTypeValidate
checkParameterList validparameters res | HS.null validparameters = Right res
                                       | otherwise = if HS.null setdiff
                                            then Right res
                                            else perror $ "Unknown parameters: " <+> list (map paramname $ HS.toList setdiff)
    where
        keyset = HS.fromList $ Map.keys (res ^. rattributes)
        setdiff = HS.difference keyset (metaparameters `HS.union` validparameters)

-- | This validator always accept the resources, but add the default parameters (to be defined :)
addDefaults :: NativeTypeValidate
addDefaults res = Right (res & rattributes %~ newparams)
    where
        def PUndef = False
        def _      = True
        newparams p = Map.filter def $ Map.union p defaults
        defaults    = Map.empty

-- | Helper function that runs a validor on a 'PArray'
runarray :: Text -> (Text -> PValue -> NativeTypeValidate) -> NativeTypeValidate
runarray param func res = case res ^. rattributes . at param of
  Just (PArray x) -> V.foldM (flip (func param)) res x
  Just x          -> perror $ "Parameter" <+> paramname param <+> "should be an array, not" <+> pretty x
  Nothing         -> Right res

{-| This checks that a given parameter is a string. If it is a 'PBoolean' or
'PNumber' it will convert them to a string.
-}
string :: Text -> NativeTypeValidate
string param res = case res ^. rattributes . at param of
  Just x  -> string' param x res
  Nothing -> Right res

strings :: Text -> NativeTypeValidate
strings param = runarray param string'

-- | Validates a string or an array of strings
string_s :: Text -> NativeTypeValidate
string_s param res = case res ^. rattributes . at param of
  Nothing         -> Right res
  Just (PArray _) -> strings param res
  Just _          -> string param res

string' :: Text -> PValue -> NativeTypeValidate
string' param rev res = case rev of
  PString _      -> Right res
  PBoolean True  -> Right (res & rattributes . at param ?~ PString "true")
  PBoolean False -> Right (res & rattributes . at param ?~ PString "false")
  PNumber n      -> Right (res & rattributes . at param ?~ PString (scientific2text n))
  x              -> perror $ "Parameter" <+> paramname param <+> "should be a string, and not" <+> pretty x

-- | Makes sure that the parameter, if defined, has a value among this list.
values :: [Text] -> Text -> NativeTypeValidate
values valuelist param res = case res ^. rattributes . at param of
    Just (PString x) -> if x `elem` valuelist
        then Right res
        else perror $ "Parameter" <+> paramname param <+> "value should be one of" <+> list (map ppline valuelist) <+> "and not" <+> ppline x
    Just x  -> perror $ "Parameter" <+> paramname param <+> "value should be one of" <+> list (map ppline valuelist) <+> "and not" <+> pretty x
    Nothing -> Right res

-- | This fills the default values of unset parameters.
defaultvalue :: Text -> Text -> NativeTypeValidate
defaultvalue value param = Right . over (rattributes . at param) (Just . fromMaybe (PString value))

-- | Checks that a given parameter, if set, is a 'PNumber'.
-- If it is a 'PString' it will attempt to parse it.
integer :: Text -> NativeTypeValidate
integer prm res = string prm res >>= integer' prm
  where
    integer' pr rs = case rs ^. rattributes . at pr of
      Just x  -> integer'' prm x res
      Nothing -> Right rs

integers :: Text -> NativeTypeValidate
integers param = runarray param integer''

integer'' :: Text -> PValue -> NativeTypeValidate
integer'' param val res = case val ^? _PValueInteger of
  Just v -> Right (res & rattributes . at param ?~ PNumber (fromIntegral v))
  _      -> perror $ "Parameter" <+> paramname param <+> "must be an integer"

-- | Copies the "name" value into the parameter if this is not set.
-- It implies the `string` validator.
nameval :: Text -> NativeTypeValidate
nameval prm res =
  string prm res >>= \r ->
    case r ^. rattributes . at prm of
      Just (PString al) -> Right (res & rid . iname .~ al)
      Just x            -> perror ("The alias must be a string, not" <+> pretty x)
      Nothing           -> Right (r & rattributes . at prm ?~ PString (r ^. rid . iname))

-- | Checks that a given parameter is set unless the resources "ensure" is set to absent
mandatoryIfNotAbsent :: Text -> NativeTypeValidate
mandatoryIfNotAbsent param res =
  case res ^. rattributes . at param of
    Just _  -> Right res
    Nothing -> case res ^. rattributes . at "ensure" of
      Just "absent" -> Right res
      _             -> perror $ "Parameter" <+> paramname param <+> "should be set."

-- | Checks that a given parameter is set.
mandatory :: Text -> NativeTypeValidate
mandatory param res = case res ^. rattributes . at param of
    Just _  -> Right res
    Nothing -> perror $ "Parameter" <+> paramname param <+> "should be set."

-- Helper that takes a list of stuff and will generate a validator.
parameterFunctions :: [(Text, [Text -> NativeTypeValidate])] -> NativeTypeValidate
parameterFunctions argrules rs = foldM parameterFunctions' rs argrules
    where
    parameterFunctions' :: Resource -> (Text, [Text -> NativeTypeValidate]) -> Either PrettyError Resource
    parameterFunctions' r (param, validationfunctions) = foldM (parameterFunctions'' param) r validationfunctions
    parameterFunctions'' :: Text -> Resource -> (Text -> NativeTypeValidate) -> Either PrettyError Resource
    parameterFunctions'' param r validationfunction = validationfunction param r

-- checks that a parameter is fully qualified
fullyQualified :: Text -> NativeTypeValidate
fullyQualified param res = case res ^. rattributes . at param of
    Just path -> fullyQualified' param path res
    Nothing   -> Right res

noTrailingSlash :: Text -> NativeTypeValidate
noTrailingSlash param res =
  case res ^. rattributes . at param of
    Just (PString x) -> if Text.last x == '/'
                          then perror ("Parameter" <+> paramname param <+> "should not have a trailing slash")
                          else Right res
    _ -> Right res

fullyQualifieds :: Text -> NativeTypeValidate
fullyQualifieds param = runarray param fullyQualified'

fullyQualified' :: Text -> PValue -> NativeTypeValidate
fullyQualified' param path res =
  case path of
    PString ("") -> perror $ "Empty path for parameter" <+> paramname param
    PString p    -> if Text.head p == '/'
                      then Right res
                      else perror $ "Path must be absolute, not" <+> ppline p <+> "for parameter" <+> paramname param
    x            -> perror $ "SHOULD NOT HAPPEN: path is not a resolved string, but" <+> pretty x <+> "for parameter" <+> paramname param

rarray :: Text -> NativeTypeValidate
rarray param res = case res ^. rattributes . at param of
    Just (PArray _) -> Right res
    Just x          -> Right $ res & rattributes . at param ?~ PArray (V.singleton x)
    Nothing         -> Right res

ipaddr :: Text -> NativeTypeValidate
ipaddr param res =
  case res ^. rattributes . at param of
    Nothing -> Right res
    Just (PString ip) ->
        if checkipv4 ip 0
          then Right res
          else perror $ "Invalid IP address for parameter" <+> paramname param
    Just x -> perror $ "Parameter" <+> paramname param <+> "should be an IP address string, not" <+> pretty x

checkipv4 :: Text -> Int -> Bool
checkipv4 _  4 = False -- means that there are more than 4 groups
checkipv4 "" _ = False -- should never get an empty string
checkipv4 ip v =
    let (cur, nxt) = Text.break (=='.') ip
        nextfunc = if Text.null nxt
            then v == 3
            else checkipv4 (Text.tail nxt) (v+1)
        goodcur = not (Text.null cur) && Text.all Char.isDigit cur && (let rcur = Text.Read.read (Text.unpack cur) :: Int in (rcur >= 0) && (rcur <= 255))
    in goodcur && nextfunc

inrange :: Integer -> Integer -> Text -> NativeTypeValidate
inrange mi ma param res =
    let va = res ^. rattributes . at param
        na = va ^? traverse . _PValueNumber
    in case (va,na) of
        (Nothing, _) -> Right res
        (_,Just v)   -> if (v >= fromIntegral mi) && (v <= fromIntegral ma)
                          then Right res
                          else perror $ "Parameter" <+> paramname param <> "'s value should be between" <+> pretty mi <+> "and" <+> pretty ma
        (Just x,_)   -> perror $ "Parameter" <+> paramname param <+> "should be an integer, and not" <+> pretty x

validateSourceOrContent :: NativeTypeValidate
validateSourceOrContent res = let
    parammap =  res ^. rattributes
    source    = Map.member "source"  parammap
    content   = Map.member "content" parammap
    in if source && content
         then perror "Source and content can't be specified at the same time"
         else Right res
