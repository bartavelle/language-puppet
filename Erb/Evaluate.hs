module Erb.Evaluate (rubyEvaluate) where

import qualified Data.Map as Map
import Puppet.Interpreter.Types
import Erb.Ruby
import Data.Maybe (catMaybes)
import Data.Either (rights)
import Control.Monad.Error
import qualified Data.Text as T
import Puppet.Utils

rubyEvaluate :: Map.Map T.Text GeneralValue -> T.Text -> [RubyStatement] -> Either String T.Text
rubyEvaluate vars ctx = foldl (evalruby vars ctx) (Right "")

evalruby :: Map.Map T.Text GeneralValue -> T.Text -> Either String T.Text -> RubyStatement -> Either String T.Text
evalruby _  _   (Left err)     _        = Left err
evalruby mp ctx (Right curstr) (Puts e) = case evalExpression mp ctx e of
    Left err -> Left err
    Right ex -> Right (curstr <> ex)

evalExpression :: Map.Map T.Text GeneralValue -> T.Text -> Expression -> Either String T.Text
evalExpression mp ctx (LookupOperation varname varindex) = do
    rvname <- evalExpression mp ctx varname
    rvindx <- evalExpression mp ctx varindex
    varvalue <- getVariable mp ctx rvname
    case varvalue of
        ResolvedArray arr -> do
            case (a2i rvindx) of
                Nothing -> throwError $ "Can't convert index to integer when resolving " ++ T.unpack rvname ++ "[" ++ T.unpack rvindx ++ "]"
                Just  i -> if length arr <= i
                    then throwError $ "Array out of bound " ++ T.unpack rvname ++ "[" ++ T.unpack rvindx ++ "]"
                    else return $ arr !! i
            return "x"
        ResolvedHash hs   -> case (filter (\(a,_) -> a == rvindx) hs) of
            []      -> throwError $ "No index " ++ T.unpack rvindx ++ " for variable " ++ show varname
            (_,x):_ -> evalValue $ Right x
        x                 -> throwError $ "Can't index variable " ++ show varname ++ ", it is " ++ show x
evalExpression _  _   (Value (Literal x))          = Right x
evalExpression mp ctx (Object (Value (Literal x))) = getVariable mp ctx x >>= evalValue . Right
evalExpression _  _   x = Left $ "Can't evaluate " ++ show x

getVariable :: Map.Map T.Text GeneralValue -> T.Text -> T.Text -> Either String ResolvedValue
getVariable mp ctx rvname =
    let vars  = map (\x -> Map.lookup x mp) [rvname, ctx <> "::" <> rvname, "::" <> rvname]
        jsts  = catMaybes vars
        rghts = rights jsts
    in do
        when (null jsts)  (throwError $ "ERB: can't resolve variable " ++ T.unpack rvname)
        when (null rghts) (throwError $ "ERB: variable " ++ T.unpack rvname ++ " value is not resolved")
        return (head rghts)

evalValue :: GeneralValue -> Either String T.Text
evalValue (Left _) = Left $ "Can't evaluate a value"
evalValue (Right (ResolvedString x)) = Right x
evalValue (Right (ResolvedInt x))    = Right $ tshow x
evalValue (Right x) = Right $ tshow x

a2i :: T.Text -> Maybe Int
a2i x = case readDecimal x of
            Right y -> Just y
            _ -> Nothing

