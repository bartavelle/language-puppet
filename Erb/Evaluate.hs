module Erb.Evaluate (rubyEvaluate, getVariable) where

import Puppet.PP
import qualified Text.PrettyPrint.ANSI.Leijen as P
import Puppet.Interpreter.PrettyPrinter()
import Puppet.Interpreter.Types
import Puppet.Interpreter.Resolve
import Erb.Ruby
import qualified Data.Text as T
import Puppet.Utils
import Control.Lens
import qualified Data.Vector as V

rubyEvaluate :: Container ScopeInformation -> T.Text -> [RubyStatement] -> Either Doc T.Text
rubyEvaluate vars ctx = foldl (evalruby vars ctx) (Right "")

evalruby :: Container ScopeInformation -> T.Text -> Either Doc T.Text -> RubyStatement -> Either Doc T.Text
evalruby _  _   (Left err)     _        = Left err
evalruby mp ctx (Right curstr) (Puts e) = case evalExpression mp ctx e of
    Left err -> Left err
    Right ex -> Right (curstr <> ex)

evalExpression :: Container ScopeInformation -> T.Text -> Expression -> Either Doc T.Text
evalExpression mp ctx (LookupOperation varname varindex) = do
    rvname <- evalExpression mp ctx varname
    rvindx <- evalExpression mp ctx varindex
    varvalue <- getVariable mp ctx rvname
    case varvalue of
        PArray arr ->
            case a2i rvindx of
                Nothing -> Left $ "Can't convert index to integer when resolving" <+> ttext rvname P.<> brackets (ttext rvindx)
                Just  i -> if V.length arr <= i
                    then Left $ "Array out of bound" <+> ttext rvname P.<> brackets (ttext rvindx)
                    else evalValue (arr V.! i)
        PHash hs   -> case hs ^. at rvindx of
                          Just x -> evalValue x
                          _ -> Left $ "Can't index variable" <+> ttext rvname <+> ", it is " <+> pretty varvalue
        _ -> Left $ "Can't index variable" <+> ttext rvname <+> ", it is " <+> pretty varvalue
evalExpression _  _   (Value (Literal x))          = Right x
evalExpression mp ctx (Object (Value (Literal x))) = getVariable mp ctx x >>= evalValue
evalExpression _  _   x = Left $ "Can't evaluate" <+> pretty x

evalValue :: PValue -> Either Doc T.Text
evalValue (PString x) = Right x
evalValue x = Right $ tshow x

a2i :: T.Text -> Maybe Int
a2i x = case readDecimal x of
            Right y -> Just y
            _ -> Nothing
