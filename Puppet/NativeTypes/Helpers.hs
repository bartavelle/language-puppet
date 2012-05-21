module Puppet.NativeTypes.Helpers where

import Puppet.Interpreter.Types
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Char (isDigit)
import Control.Monad

type PuppetTypeName = String
type PuppetTypeValidate = RResource -> Either String RResource
data PuppetTypeMethods = PuppetTypeMethods {
    puppetvalidate :: PuppetTypeValidate,
    puppetfields   :: Set.Set String
    }

faketype :: PuppetTypeName -> (PuppetTypeName, PuppetTypeMethods)
faketype tname = (tname, PuppetTypeMethods (\x -> Right x) Set.empty)

defaulttype :: PuppetTypeName -> (PuppetTypeName, PuppetTypeMethods)
defaulttype tname = (tname, PuppetTypeMethods (defaultValidate Set.empty) Set.empty)

-- this helper validates any resource :
--  adds defaults
--  checks the field list
defaultValidate :: Set.Set String -> PuppetTypeValidate
defaultValidate validparameters = checkParameterList validparameters >=> addDefaults

-- checks that there are not unknown parameters but
--  - tag
checkParameterList :: Set.Set String -> PuppetTypeValidate
checkParameterList validparameters res | Set.null validparameters = Right res
                                       | otherwise = if Set.null setdiff
                                            then Right res
                                            else Left $ "Unknown parameters " ++ (show $ Set.toList setdiff)
    where
        keyset = Map.keysSet (rrparams res)
        setdiff = Set.difference keyset (Set.insert "tag" validparameters)

-- it fills the name and title parameter if necessary
addDefaults :: PuppetTypeValidate
addDefaults res = Right (res { rrparams = newparams } )
    where
        newparams = Map.filter (/= ResolvedUndefined) $ Map.union defaults (rrparams res) 
        defaults  = Map.fromList [("name", nm),("title", nm)]
        nm = ResolvedString $ rrname res


string :: String -> PuppetTypeValidate
string param res = case (Map.lookup param (rrparams res)) of
    Just (ResolvedString _)   -> Right res
    Just (ResolvedInt n)      -> Right (insertparam res param (ResolvedString $ show n))
    Just (ResolvedBool True)  -> Right (insertparam res param (ResolvedString "true"))
    Just (ResolvedBool False) -> Right (insertparam res param (ResolvedString "false"))
    Just x                    -> Left $ "Parameter " ++ param ++ " should be a string, and not " ++ show x
    Nothing -> Right res

-- makes sure that the parameter, if defined, has a value among this list
values :: [String] -> String -> PuppetTypeValidate
values valuelist param res = case (Map.lookup param (rrparams res)) of
    Just (ResolvedString x) -> if (elem x valuelist)
        then Right res
        else Left $ "Parameter " ++ param ++ " value should be one of " ++ show valuelist
    Just _  -> Left $ "Parameter " ++ param ++ " value should be one of " ++ show valuelist
    Nothing -> Right res

defaultvalue :: String -> String -> PuppetTypeValidate
defaultvalue value param res = case (Map.lookup param (rrparams res)) of
    Just _  -> Right res
    Nothing -> Right $ insertparam res param (ResolvedString value)

insertparam :: RResource -> String -> ResolvedValue -> RResource
insertparam res param value = res { rrparams = Map.insert param value (rrparams res) }

integer :: String -> PuppetTypeValidate
integer param res = string param res >>= integer' param
    where
        integer' param res = case (Map.lookup param (rrparams res)) of
            Nothing -> Right res
            Just (ResolvedString x) -> if (all isDigit x)
                then Right $ insertparam res param (ResolvedInt $ read x)
                else Left $ "Parameter " ++ param ++ " should be a number"
            Just (ResolvedInt _) -> Right res
            _ -> Left $ "Parameter " ++ param ++ " must be an integer"

parameterFunctions :: [(String, [String -> PuppetTypeValidate])] -> PuppetTypeValidate
parameterFunctions argrules res = foldM parameterFunctions' res argrules
    where
    parameterFunctions' :: RResource -> (String, [String -> PuppetTypeValidate]) -> Either String RResource
    parameterFunctions' res (param, validationfunctions) = foldM (parameterFunctions'' param) res validationfunctions
    parameterFunctions'' :: String -> RResource -> (String -> PuppetTypeValidate) -> Either String RResource
    parameterFunctions'' param res validationfunction = validationfunction param res
