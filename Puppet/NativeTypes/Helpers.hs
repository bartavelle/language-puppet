{-| These are the function and data types that are used to define the Puppet
native types.
-}
module Puppet.NativeTypes.Helpers where

import Puppet.Interpreter.Types
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Char (isDigit)
import Control.Monad
import qualified Data.Text as T
import Puppet.Utils

faketype :: PuppetTypeName -> (PuppetTypeName, PuppetTypeMethods)
faketype tname = (tname, PuppetTypeMethods Right Set.empty)

defaulttype :: PuppetTypeName -> (PuppetTypeName, PuppetTypeMethods)
defaulttype tname = (tname, PuppetTypeMethods (defaultValidate Set.empty) Set.empty)

{-| This helper will validate resources given a list of fields. It will run
'checkParameterList' and then 'addDefaults'. -}
defaultValidate :: Set.Set T.Text -> PuppetTypeValidate
defaultValidate validparameters = checkParameterList validparameters >=> addDefaults

-- | This validator checks that no unknown parameters have been set (except metaparameters)
checkParameterList :: Set.Set T.Text -> PuppetTypeValidate
checkParameterList validparameters res | Set.null validparameters = Right res
                                       | otherwise = if Set.null setdiff
                                            then Right res
                                            else Left $ "Unknown parameters " ++ show (Set.toList setdiff)
    where
        keyset = Map.keysSet (rrparams res)
        setdiff = Set.difference keyset (Set.union metaparameters validparameters)

-- | This validator always accept the resources, but add the default parameters
-- (such as title).
addDefaults :: PuppetTypeValidate
addDefaults res = Right (res { rrparams = newparams } )
    where
        newparams = Map.filter (/= ResolvedUndefined) $ Map.union (rrparams res) defaults
        defaults  = Map.fromList [("title", nm)]
        nm = ResolvedString $ rrname res

-- | Helper function that runs a validor on a ResolvedArray
runarray :: T.Text -> (T.Text -> ResolvedValue -> PuppetTypeValidate) -> PuppetTypeValidate
runarray param func res = case Map.lookup param (rrparams res) of
    Just (ResolvedArray x) -> foldM (\res' cu -> func param cu res') res x
    Just x                 -> Left $ "Parameter " ++ T.unpack param ++ " should be an array, not " ++ show x
    Nothing                -> Right res

{-| This checks that a given parameter is a string. If it is a 'ResolvedInt' or
'ResolvedBool' it will convert them to strings.
-}
string :: T.Text -> PuppetTypeValidate
string param res = case Map.lookup param (rrparams res) of
    Just x  -> string' param x res
    Nothing -> Right res

strings :: T.Text -> PuppetTypeValidate
strings param = runarray param string'

string' :: T.Text -> ResolvedValue -> PuppetTypeValidate
string' param re res = case re of
    ResolvedString _   -> Right res
    ResolvedInt n      -> Right (insertparam res param (ResolvedString (tshow n)))
    ResolvedBool True  -> Right (insertparam res param (ResolvedString "true"))
    ResolvedBool False -> Right (insertparam res param (ResolvedString "false"))
    x                  -> Left $ "Parameter " ++ T.unpack param ++ " should be a string, and not " ++ show x

-- | Makes sure that the parameter, if defined, has a value among this list.
values :: [T.Text] -> T.Text -> PuppetTypeValidate
values valuelist param res = case (Map.lookup param (rrparams res)) of
    Just (ResolvedString x) -> if elem x valuelist
        then Right res
        else Left $ "Parameter " ++ T.unpack param ++ " value should be one of " ++ show valuelist ++ " and not " ++ T.unpack x
    Just x  -> Left $ "Parameter " ++ T.unpack param ++ " value should be one of " ++ show valuelist ++ " and not " ++ show x
    Nothing -> Right res

-- | This fills the default values of unset parameters.
defaultvalue :: T.Text -> T.Text -> PuppetTypeValidate
defaultvalue value param res = case (Map.lookup param (rrparams res)) of
    Just _  -> Right res
    Nothing -> Right $ insertparam res param (ResolvedString value)

-- | Checks that a given parameter, if set, is a 'ResolvedInt'. If it is a
-- 'ResolvedString' it will attempt to parse it.
integer :: T.Text -> PuppetTypeValidate
integer prm res = string prm res >>= integer' prm
    where
        integer' pr rs = case (Map.lookup pr (rrparams rs)) of
            Just x  -> integer'' prm x res
            Nothing -> Right rs

integers :: T.Text -> PuppetTypeValidate
integers param = runarray param integer''

integer'' :: T.Text -> ResolvedValue -> PuppetTypeValidate
integer'' param val res = case val of
    ResolvedString x -> if T.all isDigit x
        then Right $ insertparam res param (ResolvedInt $ read $ T.unpack x)
        else Left $ "Parameter " ++ T.unpack param ++ " should be a number"
    ResolvedInt _ -> Right res
    _ -> Left $ "Parameter " ++ T.unpack param ++ " must be an integer"

-- | Copies the "name" value into the parameter if this is not set. It implies
-- the `string` validator.
nameval :: T.Text -> PuppetTypeValidate
nameval prm res = do
    nres <- string prm res >>= addAlias' prm
    case Map.lookup "title" (rrparams res) of
        Just (ResolvedString nm) -> defaultvalue nm prm nres >>= string prm
        Just x                   -> Left $ "The title should be a string, and not " ++ show x
        Nothing                  -> Left "The title is not set. Not sure how that could ever happen."

addAlias' :: T.Text -> PuppetTypeValidate
addAlias' prm res = case Map.lookup prm (rrparams res) of
                       Just (ResolvedString nm) -> addAlias nm res
                       Just _  -> Left "Can't happen, the nameval should have been a string"
                       Nothing -> return res

-- | Checks that a given parameter is set.
mandatory :: T.Text -> PuppetTypeValidate
mandatory param res = case Map.lookup param (rrparams res) of
    Just _  -> Right res
    Nothing -> Left $ "Parameter " ++ T.unpack param ++ " should be set."

-- | Helper that takes a list of stuff and will generate a validator.
parameterFunctions :: [(T.Text, [T.Text -> PuppetTypeValidate])] -> PuppetTypeValidate
parameterFunctions argrules rs = foldM parameterFunctions' rs argrules
    where
    parameterFunctions' :: RResource -> (T.Text, [T.Text -> PuppetTypeValidate]) -> Either String RResource
    parameterFunctions' r (param, validationfunctions) = foldM (parameterFunctions'' param) r validationfunctions
    parameterFunctions'' :: T.Text -> RResource -> (T.Text -> PuppetTypeValidate) -> Either String RResource
    parameterFunctions'' param r validationfunction = validationfunction param r

-- checks that a parameter is fully qualified
fullyQualified :: T.Text -> PuppetTypeValidate
fullyQualified param res = case Map.lookup param (rrparams res) of
    Just path -> fullyQualified' param path res
    Nothing -> Right res

noTrailingSlash :: T.Text -> PuppetTypeValidate
noTrailingSlash param res = case Map.lookup param (rrparams res) of
     Just (ResolvedString x) -> if T.last x == '/'
                                    then Left ("Parameter " ++ T.unpack param ++ " should not have a trailing slash")
                                    else Right res
     _ -> Right res

fullyQualifieds :: T.Text -> PuppetTypeValidate
fullyQualifieds param = runarray param fullyQualified'

fullyQualified' :: T.Text -> ResolvedValue -> PuppetTypeValidate
fullyQualified' param path res = case path of
    ResolvedString ("")    -> Left $ "Empty path for parameter " ++ T.unpack param
    ResolvedString p -> if T.head p == '/'
                            then Right res
                            else Left $ "Path must be absolute, not " ++ T.unpack p ++ " for parameter " ++ T.unpack param
    x                -> Left $ "SHOULD NOT HAPPEN: path is not a resolved string, but " ++ show x ++ " for parameter " ++ show x

rarray :: T.Text -> PuppetTypeValidate
rarray param res = case Map.lookup param (rrparams res) of
    Just (ResolvedArray _) -> Right res
    Just x                 -> Right $ insertparam res param (ResolvedArray [x])
    Nothing                -> Right res

ipaddr :: T.Text -> PuppetTypeValidate
ipaddr param res = case Map.lookup param (rrparams res) of
    Nothing                  -> Right res
    Just (ResolvedString ip) ->
        if checkipv4 ip 0
            then Right res
            else Left $ "Invalid IP address for parameter " ++ T.unpack param
    Just x -> Left $ "Parameter " ++ T.unpack param ++ " should be an IP address string, not " ++ show x

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
inrange mi ma param res = case Map.lookup param (rrparams res) of
    Nothing                 -> Right res
    Just (ResolvedInt v)    -> if (v>=mi) && (v<=ma)
                                then Right res
                                else Left $ "Parameter " ++ T.unpack param ++ "'s value should be between " ++ show mi ++ " and " ++ show ma
    Just x                  -> Left $ "Parameter " ++ T.unpack param ++ " should be an integer, and not " ++ show x

