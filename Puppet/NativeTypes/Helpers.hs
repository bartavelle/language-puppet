{-| These are the function and data types that are used to define the Puppet
native types.
-}
module Puppet.NativeTypes.Helpers 
    ( module Puppet.PP
    , ipaddr
    , paramname
    , rarray
    , string
    , strings
    , noTrailingSlash
    , fullyQualified
    , fullyQualifieds
    , values
    , defaultvalue
    , nameval
    , defaultValidate
    , PuppetTypeName
    , parameterFunctions
    , integer
    , integers
    , mandatory
    , inrange
    , faketype
    , defaulttype
    ) where

import Puppet.PP hiding (string,integer)
import qualified Text.PrettyPrint.ANSI.Leijen as P
import Puppet.Interpreter.Types
import Puppet.Interpreter.PrettyPrinter()
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Char (isDigit)
import Control.Monad
import qualified Data.Text as T
import Puppet.Utils
import Control.Lens
import qualified Data.Vector as V

type PuppetTypeName = T.Text

paramname :: T.Text -> Doc
paramname = red . ttext

faketype :: PuppetTypeName -> (PuppetTypeName, PuppetTypeMethods)
faketype tname = (tname, PuppetTypeMethods Right HS.empty)

defaulttype :: PuppetTypeName -> (PuppetTypeName, PuppetTypeMethods)
defaulttype tname = (tname, PuppetTypeMethods (defaultValidate HS.empty) HS.empty)

{-| This helper will validate resources given a list of fields. It will run
'checkParameterList' and then 'addDefaults'. -}
defaultValidate :: HS.HashSet T.Text -> PuppetTypeValidate
defaultValidate validparameters = checkParameterList validparameters >=> addDefaults

-- | This validator checks that no unknown parameters have been set (except metaparameters)
checkParameterList :: HS.HashSet T.Text -> PuppetTypeValidate
checkParameterList validparameters res | HS.null validparameters = Right res
                                       | otherwise = if HS.null setdiff
                                            then Right res
                                            else Left $ "Unknown parameters: " <+> list (map paramname $ HS.toList setdiff)
    where
        keyset = HS.fromList $ HM.keys (res ^. rattributes)
        setdiff = HS.difference keyset (metaparameters `HS.union` validparameters)

-- | This validator always accept the resources, but add the default parameters
-- (such as title).
addDefaults :: PuppetTypeValidate
addDefaults res = Right (res & rattributes %~ newparams)
    where
        def PUndef = False
        def _ = True
        newparams p = HM.filter def $ HM.union p defaults
        defaults    = HM.fromList [("title", PString (res ^. rid ^. iname))]

-- | Helper function that runs a validor on a PArray
runarray :: T.Text -> (T.Text -> PValue -> PuppetTypeValidate) -> PuppetTypeValidate
runarray param func res = case res ^. rattributes . at param of
    Just (PArray x) -> V.foldM (flip (func param)) res x
    Just x          -> Left $ "Parameter" <+> paramname param <+> "should be an array, not" <+> pretty x
    Nothing         -> Right res

{-| This checks that a given parameter is a string. If it is a 'ResolvedInt' or
'ResolvedBool' it will convert them to strings.
-}
string :: T.Text -> PuppetTypeValidate
string param res = case res ^. rattributes . at param of
    Just x  -> string' param x res
    Nothing -> Right res

strings :: T.Text -> PuppetTypeValidate
strings param = runarray param string'

string' :: T.Text -> PValue -> PuppetTypeValidate
string' param rev res = case rev of
    PString _      -> Right res
    PBoolean True  -> Right (res & rattributes . at param ?~ PString "true")
    PBoolean False -> Right (res & rattributes . at param ?~ PString "false")
    x              -> Left $ "Parameter" <+> paramname param <+> "should be a string, and not" <+> pretty x

-- | Makes sure that the parameter, if defined, has a value among this list.
values :: [T.Text] -> T.Text -> PuppetTypeValidate
values valuelist param res = case res ^. rattributes . at param of
    Just (PString x) -> if x `elem` valuelist
        then Right res
        else Left $ "Parameter" <+> paramname param <+> "value should be one of" <+> list (map ttext valuelist) <+> "and not" <+> ttext x
    Just x  -> Left $ "Parameter" <+> paramname param <+> "value should be one of" <+> list (map ttext valuelist) <+> "and not" <+> pretty x
    Nothing -> Right res

-- | This fills the default values of unset parameters.
defaultvalue :: T.Text -> T.Text -> PuppetTypeValidate
defaultvalue value param res = case res ^. rattributes . at param of
    Just _  -> Right res
    Nothing -> Right $ res & rattributes . at param ?~ PString value

-- | Checks that a given parameter, if set, is a 'ResolvedInt'. If it is a
-- 'PString' it will attempt to parse it.
integer :: T.Text -> PuppetTypeValidate
integer prm res = string prm res >>= integer' prm
    where
        integer' pr rs = case rs ^. rattributes . at pr of
            Just x  -> integer'' prm x res
            Nothing -> Right rs

integers :: T.Text -> PuppetTypeValidate
integers param = runarray param integer''

integer'' :: T.Text -> PValue -> PuppetTypeValidate
integer'' param val res = case val of
    PString x -> if T.all isDigit x
        then Right res
        else Left $ "Parameter" <+> paramname param <+> "should be an integer"
    _ -> Left $ "Parameter" <+> paramname param <+> "must be an integer"

-- | Copies the "name" value into the parameter if this is not set. It implies
-- the `string` validator.
nameval :: T.Text -> PuppetTypeValidate
nameval prm res = string prm res
                    >>= \r -> case r ^. rattributes . at prm of
                                  Just (PString al) -> Right (res & rid . iname .~ al)
                                  Just x -> Left ("The alias must be a string, not" <+> pretty x)
                                  Nothing -> Right (r & rattributes . at prm ?~ PString (r ^. rid . iname))

-- | Checks that a given parameter is set.
mandatory :: T.Text -> PuppetTypeValidate
mandatory param res = case res ^. rattributes . at param of
    Just _  -> Right res
    Nothing -> Left $ "Parameter" <+> paramname param <+> "should be set."

-- | Helper that takes a list of stuff and will generate a validator.
parameterFunctions :: [(T.Text, [T.Text -> PuppetTypeValidate])] -> PuppetTypeValidate
parameterFunctions argrules rs = foldM parameterFunctions' rs argrules
    where
    parameterFunctions' :: Resource -> (T.Text, [T.Text -> PuppetTypeValidate]) -> Either Doc Resource
    parameterFunctions' r (param, validationfunctions) = foldM (parameterFunctions'' param) r validationfunctions
    parameterFunctions'' :: T.Text -> Resource -> (T.Text -> PuppetTypeValidate) -> Either Doc Resource
    parameterFunctions'' param r validationfunction = validationfunction param r

-- checks that a parameter is fully qualified
fullyQualified :: T.Text -> PuppetTypeValidate
fullyQualified param res = case res ^. rattributes . at param of
    Just path -> fullyQualified' param path res
    Nothing -> Right res

noTrailingSlash :: T.Text -> PuppetTypeValidate
noTrailingSlash param res = case res ^. rattributes . at param of
     Just (PString x) -> if T.last x == '/'
                                    then Left ("Parameter" <+> paramname param <+> "should not have a trailing slash")
                                    else Right res
     _ -> Right res

fullyQualifieds :: T.Text -> PuppetTypeValidate
fullyQualifieds param = runarray param fullyQualified'

fullyQualified' :: T.Text -> PValue -> PuppetTypeValidate
fullyQualified' param path res = case path of
    PString ("")    -> Left $ "Empty path for parameter" <+> paramname param
    PString p -> if T.head p == '/'
                            then Right res
                            else Left $ "Path must be absolute, not" <+> ttext p <+> "for parameter" <+> paramname param
    x                -> Left $ "SHOULD NOT HAPPEN: path is not a resolved string, but" <+> pretty x <+> "for parameter" <+> paramname param

rarray :: T.Text -> PuppetTypeValidate
rarray param res = case res ^. rattributes . at param of
    Just (PArray _) -> Right res
    Just x          -> Right $ res & rattributes . at param ?~ PArray (V.singleton x)
    Nothing         -> Right res

ipaddr :: T.Text -> PuppetTypeValidate
ipaddr param res = case res ^. rattributes . at param of
    Nothing                  -> Right res
    Just (PString ip) ->
        if checkipv4 ip 0
            then Right res
            else Left $ "Invalid IP address for parameter" <+> paramname param
    Just x -> Left $ "Parameter" <+> paramname param <+> "should be an IP address string, not" <+> pretty x

checkipv4 :: T.Text -> Int -> Bool
checkipv4 _  4 = False -- means that there are more than 4 groups
checkipv4 "" _ = False -- should never get an empty string
checkipv4 ip v =
    let (cur, nxt) = T.break (=='.') ip
        nextfunc = if T.null nxt
            then v == 3
            else checkipv4 (T.tail nxt) (v+1)
        goodcur = not (T.null cur) && T.all isDigit cur && (let rcur = read (T.unpack cur) :: Int in (rcur >= 0) && (rcur <= 255))
    in goodcur && nextfunc

inrange :: Integer -> Integer -> T.Text -> PuppetTypeValidate
inrange mi ma param res =
    let va = res ^. rattributes . at param
        na = va >>= puppet2number
    in case (va,na) of
        (Nothing, _)       -> Right res
        (_,Just (Left v))  -> if (v >= fromIntegral mi) && (v <= fromIntegral ma)
                                    then Right res
                                    else Left $ "Parameter" <+> paramname param P.<> "'s value should be between" <+> P.integer mi <+> "and" <+> P.integer ma
        (_,Just (Right v)) -> if (v>=mi) && (v<=ma)
                                    then Right res
                                    else Left $ "Parameter" <+> paramname param P.<> "'s value should be between" <+> P.integer mi <+> "and" <+> P.integer ma
        (Just x,_)         -> Left $ "Parameter" <+> paramname param <+> "should be an integer, and not" <+> pretty x

