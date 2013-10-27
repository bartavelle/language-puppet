{-# LANGUAGE LambdaCase #-}
module Puppet.Stdlib (stdlibFunctions) where

import Puppet.PP
import Puppet.Parser.Types
import Puppet.Interpreter.Resolve
import Puppet.Interpreter.Types

import Control.Lens
import Data.Char
import Data.Monoid
import Control.Monad
import Control.Monad.IO.Class
import Text.Regex.PCRE.ByteString
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Attoparsec.Number
import qualified Data.ByteString.Base16 as B16

stdlibFunctions :: Container ( [PValue] -> InterpreterMonad PValue )
stdlibFunctions = HM.fromList [ ("abs", puppetAbs)
                              , ("any2array", any2array)
                              , ("base64", base64)
                              , ("bool2num", bool2num)
                              , ("capitalize", stringArrayFunction (safeEmptyString (\t -> T.cons (toUpper (T.head t)) (T.tail t))))
                              , ("chomp", stringArrayFunction (T.dropWhileEnd (\c -> c == '\n' || c == '\r')))
                              , ("chop", stringArrayFunction (safeEmptyString T.init))
                              , ("concat", puppetConcat)
                              , ("count", puppetCount)
                              , ("defined_with_params", const (throwPosError "defined_with_params can't be implemented with language-puppet"))
                              , ("delete", delete)
                              , ("delete_at", deleteAt)
                              , ("delete_undef_values", deleteUndefValues)
                              , ("downcase", stringArrayFunction T.toLower)
                              , ("getvar", getvar)
                              , ("is_domain_name", isDomainName)
                              , ("is_integer", isInteger)
                              , ("keys", keys)
                              , ("lstrip", stringArrayFunction T.stripStart)
                              , ("merge", merge)
                              , ("rstrip", stringArrayFunction T.stripEnd)
                              , ("strip", stringArrayFunction T.strip)
                              , ("upcase", stringArrayFunction T.toUpper)
                              , ("validate_array", validateArray)
                              , ("validate_bool", validateBool)
                              , ("validate_hash", validateHash)
                              , ("validate_re", validateRe)
                              , ("validate_string", validateString)
                              ]

safeEmptyString :: (T.Text -> T.Text) -> T.Text -> T.Text
safeEmptyString _ "" = ""
safeEmptyString f x = f x

stringArrayFunction :: (T.Text -> T.Text) -> [PValue] -> InterpreterMonad PValue
stringArrayFunction f [PString s] = return (PString (f s))
stringArrayFunction f [PArray xs] = fmap PArray (V.mapM (fmap (PString . f) . resolvePValueString) xs)
stringArrayFunction _ [a] = throwPosError ("function expects a string or an array of strings, not" <+> pretty a)
stringArrayFunction _ _ = throwPosError "function expects a single argument"


compileRE :: T.Text -> InterpreterMonad Regex
compileRE p =
    (liftIO . compile compBlank execBlank . T.encodeUtf8) p >>= \case
        Right r -> return r
        Left ms -> throwPosError ("Can't parse regexp" <+> pretty (URegexp p undefined) <+> ":" <+> text (show ms))

puppetAbs :: [PValue] -> InterpreterMonad PValue
puppetAbs [y] = case y ^? pvnum of
                     Just (I x) -> return $ pvnum # I (abs x)
                     Just (D x) -> return $ pvnum # D (abs x)
                     Nothing -> throwPosError ("abs(): Expects a number, not" <+> pretty y)
puppetAbs _ = throwPosError "abs(): Takes a single argument"

any2array :: [PValue] -> InterpreterMonad PValue
any2array [PArray v] = return (PArray v)
any2array [PHash h] = return (PArray lst)
    where lst = V.fromList $ concatMap arraypair $ HM.toList h
          arraypair (a,b) = [PString a, b]
any2array [x] = return (PArray (V.singleton x))
any2array x = return (PArray (V.fromList x))

base64 :: [PValue] -> InterpreterMonad PValue
base64 [pa,pb] = do
    b <- fmap T.encodeUtf8 (resolvePValueString pb)
    r <- resolvePValueString pa >>= \case
        "encode" -> return (B16.encode b)
        "decode" -> case B16.decode b of
                        (x, "") -> return x
                        _       -> throwPosError ("base64(): could not decode" <+> pretty pb)
        a        -> throwPosError ("base64(): the first argument must be either 'encode' or 'decode', not" <+> ttext a)
    fmap PString (safeDecodeUtf8 r)

base64 _ = throwPosError "base64(): Expects 2 arguments"

bool2num :: [PValue] -> InterpreterMonad PValue
bool2num [PString ""] = return (PBoolean False)
bool2num [PString "1"] = return (PBoolean True)
bool2num [PString "t"] = return (PBoolean True)
bool2num [PString "y"] = return (PBoolean True)
bool2num [PString "true"] = return (PBoolean True)
bool2num [PString "yes"] = return (PBoolean True)
bool2num [PString "0"] = return (PBoolean False)
bool2num [PString "f"] = return (PBoolean False)
bool2num [PString "n"] = return (PBoolean False)
bool2num [PString "false"] = return (PBoolean False)
bool2num [PString "no"] = return (PBoolean False)
bool2num [PString "undef"] = return (PBoolean False)
bool2num [PString "undefined"] = return (PBoolean False)
bool2num [x@(PBoolean _)] = return x
bool2num [x] = throwPosError ("bool2num(): Can't convert" <+> pretty x <+> "to boolean")
bool2num _ = throwPosError "bool2num() expects a single argument"

puppetConcat :: [PValue] -> InterpreterMonad PValue
puppetConcat [PArray a, PArray b] = return (PArray (a <> b))
puppetConcat [a,b] = throwPosError ("concat(): both arguments must be arrays, not" <+> pretty a <+> "or" <+> pretty b)
puppetConcat _ = throwPosError "concat(): expects 2 arguments"

puppetCount :: [PValue] -> InterpreterMonad PValue
puppetCount [PArray x] = return (pvnum # I (V.foldl' cnt 0 x))
    where
        cnt cur (PString "") = cur
        cnt cur PUndef = cur
        cnt cur _ = cur + 1
puppetCount [PArray x, y] = return (pvnum # I (V.foldl' cnt 0 x))
    where
        cnt cur z | y == z = cur + 1
                  | otherwise = cur
puppetCount _ = throwPosError "count(): expects 1 or 2 arguments"

delete :: [PValue] -> InterpreterMonad PValue
delete [PString x, y] = do
    ty <- resolvePValueString y
    return $ PString $ T.concat $ T.splitOn ty x
delete [PArray r, z] = return $ PArray $ V.filter (/= z) r
delete [PHash h, z] = do
   tz <- resolvePValueString z
   return $ PHash (h & at tz .~ Nothing)
delete [a,_] = throwPosError ("delete(): First argument must be an Array, String, or Hash. Given:" <+> pretty a)
delete _ = throwPosError "delete(): expects 2 arguments"

deleteAt :: [PValue] -> InterpreterMonad PValue
deleteAt [PArray r, z] = case z ^? pvnum of
                              Just (I gn) ->
                                let n = fromInteger gn
                                    lr = V.length r
                                    s1 = V.slice 0 n r
                                    s2 = V.slice (n+1) (lr - n - 1) r
                                in  if V.length r >= n
                                       then throwPosError ("delete_at(): Out of bounds access detected, tried to remove index" <+> pretty z <+> "wheras the array only has" <+> string (show lr) <+> "elements")
                                       else return (PArray (s1 <> s2))
                              _ -> throwPosError ("delete_at(): The second argument must be an integer, not" <+> pretty z)
deleteAt [x,_] = throwPosError ("delete_at(): expects its first argument to be an array, not" <+> pretty x)
deleteAt _ = throwPosError "delete_at(): expects 2 arguments"

deleteUndefValues :: [PValue] -> InterpreterMonad PValue
deleteUndefValues [PArray r] = return $ PArray $ V.filter (/= PUndef) r
deleteUndefValues [PHash h] = return $ PHash $ HM.filter (/= PUndef) h
deleteUndefValues [x] = throwPosError ("delete_undef_values(): Expects an Array or a Hash, not" <+> pretty x)
deleteUndefValues _ = throwPosError "delete_undef_values(): Expects a single argument"

getvar :: [PValue] -> InterpreterMonad PValue
getvar [x] = resolvePValueString x >>= resolveVariable
getvar _ = throwPosError "getvar() expects a single argument"


isDomainName :: [PValue] -> InterpreterMonad PValue
isDomainName [s] = do
    rs <- resolvePValueString s
    let ndrs = if T.last rs == '.'
                   then T.init rs
                   else rs
        prts = T.splitOn "." ndrs
        checkPart x = not (T.null x)
                        && (T.length x <= 63)
                        && (T.head x /= '-')
                        && (T.last x /= '-')
                        && T.all (\y -> isAlphaNum y || y == '-') x
    return $ PBoolean $ not (T.null rs) && T.length rs <= 255 && all checkPart prts
isDomainName _ = throwPosError "is_domain_name(): Should only take a single argument"

isInteger :: [PValue] -> InterpreterMonad PValue
isInteger [i] = return (PBoolean (not (isn't pvnum i)))
isInteger _ = throwPosError "is_integer(): Should only take a single argument"

keys :: [PValue] -> InterpreterMonad PValue
keys [PHash h] = return (PArray $ V.fromList $ map PString $ HM.keys h)
keys [x] = throwPosError ("keys(): Expects a Hash, not" <+> pretty x)
keys _ = throwPosError "keys(): expects a single argument"

merge :: [PValue] -> InterpreterMonad PValue
merge [PHash a, PHash b] = return (PHash (b `HM.union` a))
merge [a,b] = throwPosError ("merge(): Expects two hashes, not" <+> pretty a <+> pretty b)
merge _ = throwPosError "merge(): Expects two hashes"

validateArray :: [PValue] -> InterpreterMonad PValue
validateArray [] = throwPosError "validate_array(): wrong number of arguments, must be > 0"
validateArray x = mapM_ vb x >> return PUndef
    where
        vb (PArray _) = return ()
        vb y = throwPosError (pretty y <+> "is not an array.")

validateBool :: [PValue] -> InterpreterMonad PValue
validateBool [] = throwPosError "validate_bool(): wrong number of arguments, must be > 0"
validateBool x = mapM_ vb x >> return PUndef
    where
        vb (PBoolean _) = return ()
        vb y = throwPosError (pretty y <+> "is not a boolean.")

validateHash :: [PValue] -> InterpreterMonad PValue
validateHash [] = throwPosError "validate_hash(): wrong number of arguments, must be > 0"
validateHash x = mapM_ vb x >> return PUndef
    where
        vb (PHash _) = return ()
        vb y = throwPosError (pretty y <+> "is not a hash.")

validateRe :: [PValue] -> InterpreterMonad PValue
validateRe [str, reg] = validateRe [str, reg, PString "Match failed"]
validateRe [str, PString reg, msg] = validateRe [str, PArray (V.singleton (PString reg)), msg]
validateRe [str, PArray v, msg] = do
    rstr <- fmap T.encodeUtf8 (resolvePValueString str)
    let matchRE :: Regex -> InterpreterMonad Bool
        matchRE r = liftIO (execute r rstr) >>= \case
                        Left rr -> throwPosError ("Regexp matching critical failure" <+> text (show rr))
                        Right Nothing -> return False
                        _ -> return True
    rest <- mapM (resolvePValueString >=> compileRE >=> matchRE) (V.toList v)
    if or rest
        then return PUndef
        else throwPosError (pretty msg <$> "Source string:" <+> pretty str <> comma <+> "regexps:" <+> pretty (V.toList v))
validateRe [_, r, _] = throwPosError ("validate_re(): expected a regexp or an array of regexps, but not" <+> pretty r)
validateRe _ = throwPosError "validate_re(): wrong number of arguments (#{args.length}; must be 2 or 3)"

validateString :: [PValue] -> InterpreterMonad PValue
validateString [] = throwPosError "validate_string(): wrong number of arguments, must be > 0"
validateString x = mapM_ resolvePValueString x >> return PUndef
