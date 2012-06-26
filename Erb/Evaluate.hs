module Erb.Evaluate (rubyEvaluate) where

import qualified Data.Map as Map
import Puppet.Interpreter.Types
import Erb.Ruby
import Data.Maybe (catMaybes)
import Data.Either (rights)
import Control.Monad.Error
import Data.Char

rubyEvaluate :: Map.Map String GeneralValue -> String -> [RubyStatement] -> Either String String
rubyEvaluate vars ctx = foldl (evalruby vars ctx) (Right "")

evalruby :: Map.Map String GeneralValue -> String -> Either String String -> RubyStatement -> Either String String
evalruby _  _   (Left err)     _        = Left err
evalruby mp ctx (Right curstr) (Puts e) = case evalExpression mp ctx e of
    Left err -> Left err
    Right ex -> Right (curstr ++ ex)

evalExpression :: Map.Map String GeneralValue -> String -> Expression -> Either String String
evalExpression mp ctx (LookupOperation varname varindex) = do
    rvname <- evalExpression mp ctx varname
    rvindx <- evalExpression mp ctx varindex
    varvalue <- getVariable mp ctx rvname
    case varvalue of
        ResolvedArray arr -> do
            case (a2i rvindx) of
                Nothing -> throwError $ "Can't convert index to integer when resolving " ++ rvname ++ "[" ++ rvindx ++ "]"
                Just  i -> if length arr <= i
                    then throwError $ "Array out of bound " ++ rvname ++ "[" ++ rvindx ++ "]"
                    else return $ arr !! i
            return "x"
        ResolvedHash hs   -> case (filter (\(a,_) -> a == rvindx) hs) of
            []      -> throwError $ "No index " ++ rvindx ++ " for variable " ++ show varname
            (_,x):_ -> evalValue $ Right x
        x                 -> throwError $ "Can't index variable " ++ show varname ++ ", it is " ++ show x
evalExpression _  _   (Value (Literal x))          = Right x
evalExpression mp ctx (Object (Value (Literal x))) = getVariable mp ctx x >>= evalValue . Right
evalExpression _  _   x = Left $ "Can't evaluate " ++ show x

getVariable :: Map.Map String GeneralValue -> String -> String -> Either String ResolvedValue
getVariable mp ctx rvname =
    let vars = map (\x -> Map.lookup x mp) [rvname, ctx ++ "::" ++ rvname]
        jsts = catMaybes vars
        rghts = rights jsts
    in do
        when (null jsts) (throwError $ "Can't resolve variable " ++ rvname)
        when (null rghts) (throwError $ "Variable " ++ rvname ++ " value is not resolved")
        return (head rghts)
        

evalValue :: GeneralValue -> Either String String
evalValue (Left _) = Left $ "Can't evaluate a value"
evalValue (Right (ResolvedString x)) = Right x
evalValue (Right (ResolvedInt x))    = Right $ show x
evalValue (Right x) = Right $ show x

a2i :: String -> Maybe Int
a2i x | all isDigit x = read x
      | otherwise = Nothing
