{-# LANGUAGE LambdaCase #-}
module Puppet.Stdlib (stdlibFunctions) where

import Puppet.PP
import Puppet.Interpreter.Resolve
import Puppet.Interpreter.Types

import Control.Lens
import Data.Aeson.Lens
import Puppet.Lens
import Data.Char
import Data.Monoid
import Control.Monad
import Data.Vector.Lens
import Text.Regex.PCRE.ByteString.Utils
import Data.Traversable (for)
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Base16 as B16

-- | Contains the implementation of the StdLib functions.
stdlibFunctions :: Container ( [PValue] -> InterpreterMonad PValue )
stdlibFunctions = HM.fromList [ singleArgument "abs" puppetAbs
                              , ("any2array", any2array)
                              , ("base64", base64)
                              , singleArgument "bool2num" bool2num
                              , ("capitalize", stringArrayFunction (safeEmptyString (\t -> T.cons (toUpper (T.head t)) (T.tail t))))
                              , ("chomp", stringArrayFunction (T.dropWhileEnd (\c -> c == '\n' || c == '\r')))
                              , ("chop", stringArrayFunction (safeEmptyString T.init))
                              , ("concat", puppetConcat)
                              , ("count", puppetCount)
                              , ("defined_with_params", const (throwPosError "defined_with_params can't be implemented with language-puppet"))
                              , ("delete", delete)
                              , ("delete_at", deleteAt)
                              , singleArgument "delete_undef_values" deleteUndefValues
                              , ("downcase", stringArrayFunction T.toLower)
                              , singleArgument "empty" _empty
                              , singleArgument "flatten" flatten
                              , singleArgument "getvar"  getvar
                              , ("getparam", const $ throwPosError "The getparam function is uncool and shall not be implemented in language-puppet")
                              , ("grep", _grep)
                              , singleArgument "is_array" isArray
                              , singleArgument "is_domain_name" isDomainName
                              , singleArgument "is_integer" isInteger
                              , singleArgument "is_hash" isHash
                              , singleArgument "is_string" isString
                              , singleArgument "keys" keys
                              , ("has_key", hasKey)
                              , ("lstrip", stringArrayFunction T.stripStart)
                              , ("merge", merge)
                              , ("pick", pick)
                              , ("rstrip", stringArrayFunction T.stripEnd)
                              , singleArgument "size" size
                              , singleArgument "str2bool" str2Bool
                              , ("strip", stringArrayFunction T.strip)
                              , ("upcase", stringArrayFunction T.toUpper)
                              , ("validate_absolute_path", validateAbsolutePath)
                              , ("validate_array", validateArray)
                              , ("validate_bool", validateBool)
                              , ("validate_hash", validateHash)
                              , ("validate_re", validateRe)
                              , ("validate_string", validateString)
                              , singleArgument "values" pvalues
                              ]

singleArgument :: T.Text -> (PValue -> InterpreterMonad PValue) -> (T.Text, [PValue] -> InterpreterMonad PValue )
singleArgument fname ifunc = (fname, ofunc)
    where
        ofunc [x] = ifunc x
        ofunc _ = throwPosError (ttext fname <> "(): Expects a single argument.")

safeEmptyString :: (T.Text -> T.Text) -> T.Text -> T.Text
safeEmptyString _ "" = ""
safeEmptyString f x = f x

stringArrayFunction :: (T.Text -> T.Text) -> [PValue] -> InterpreterMonad PValue
stringArrayFunction f [PString s] = return (PString (f s))
stringArrayFunction f [PArray xs] = fmap PArray (V.mapM (fmap (PString . f) . resolvePValueString) xs)
stringArrayFunction _ [a] = throwPosError ("function expects a string or an array of strings, not" <+> pretty a)
stringArrayFunction _ _ = throwPosError "function expects a single argument"

compileRE :: T.Text -> InterpreterMonad Regex
compileRE r = case compile' compBlank execBlank (T.encodeUtf8 r) of
                  Left rr -> throwPosError ("Could not compile" <+> ttext r <+> ":" <+> string (show rr))
                  Right x -> return x

matchRE :: Regex -> T.Text -> InterpreterMonad Bool
matchRE r t = case execute' r (T.encodeUtf8 t) of
                  Left rr -> throwPosError ("Could not match:" <+> string (show rr))
                  Right m -> return (has _Just m)

puppetAbs :: PValue -> InterpreterMonad PValue
puppetAbs y = case y ^? _Number of
                  Just x -> return $ _Number # abs x
                  Nothing -> throwPosError ("abs(): Expects a number, not" <+> pretty y)

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

bool2num :: PValue -> InterpreterMonad PValue
bool2num (PString "") = return (PBoolean False)
bool2num (PString "1") = return (PBoolean True)
bool2num (PString "t") = return (PBoolean True)
bool2num (PString "y") = return (PBoolean True)
bool2num (PString "true") = return (PBoolean True)
bool2num (PString "yes") = return (PBoolean True)
bool2num (PString "0") = return (PBoolean False)
bool2num (PString "f") = return (PBoolean False)
bool2num (PString "n") = return (PBoolean False)
bool2num (PString "false") = return (PBoolean False)
bool2num (PString "no") = return (PBoolean False)
bool2num (PString "undef") = return (PBoolean False)
bool2num (PString "undefined") = return (PBoolean False)
bool2num x@(PBoolean _) = return x
bool2num x = throwPosError ("bool2num(): Can't convert" <+> pretty x <+> "to boolean")

puppetConcat :: [PValue] -> InterpreterMonad PValue
puppetConcat [PArray a, PArray b] = return (PArray (a <> b))
puppetConcat [a,b] = throwPosError ("concat(): both arguments must be arrays, not" <+> pretty a <+> "or" <+> pretty b)
puppetConcat _ = throwPosError "concat(): expects 2 arguments"

puppetCount :: [PValue] -> InterpreterMonad PValue
puppetCount [PArray x] = return (_Integer # V.foldl' cnt 0 x)
    where
        cnt cur (PString "") = cur
        cnt cur PUndef = cur
        cnt cur _ = cur + 1
puppetCount [PArray x, y] = return (_Integer # V.foldl' cnt 0 x)
    where
        cnt cur z | y == z = cur + 1
                  | otherwise = cur
puppetCount _ = throwPosError "count(): expects 1 or 2 arguments"

delete :: [PValue] -> InterpreterMonad PValue
delete [PString x, y] = fmap (PString . T.concat . (`T.splitOn` x)) (resolvePValueString y)
delete [PArray r, z] = return $ PArray $ V.filter (/= z) r
delete [PHash h, z] = do
   tz <- resolvePValueString z
   return $ PHash (h & at tz .~ Nothing)
delete [a,_] = throwPosError ("delete(): First argument must be an Array, String, or Hash. Given:" <+> pretty a)
delete _ = throwPosError "delete(): expects 2 arguments"

deleteAt :: [PValue] -> InterpreterMonad PValue
deleteAt [PArray r, z] = case z ^? _Integer of
                              Just gn ->
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

deleteUndefValues :: PValue -> InterpreterMonad PValue
deleteUndefValues (PArray r) = return $ PArray $ V.filter (/= PUndef) r
deleteUndefValues (PHash h) = return $ PHash $ HM.filter (/= PUndef) h
deleteUndefValues x = throwPosError ("delete_undef_values(): Expects an Array or a Hash, not" <+> pretty x)

_empty :: PValue -> InterpreterMonad PValue
_empty = return . PBoolean . flip elem [PUndef, PString "", PString "undef", PArray V.empty, PHash HM.empty] 

flatten :: PValue -> InterpreterMonad PValue
flatten r@(PArray _) = return $ PArray (flatten' r)
    where
        flatten' :: PValue -> V.Vector PValue
        flatten' (PArray x) = V.concatMap flatten' x
        flatten' x = V.singleton x
flatten x = throwPosError ("flatten(): Expects an Array, not" <+> pretty x)

getvar :: PValue -> InterpreterMonad PValue
getvar = resolvePValueString >=> resolveVariable

_grep :: [PValue] -> InterpreterMonad PValue
_grep [PArray vls, rawre] = do
    regexp <- resolvePValueString rawre >>= compileRE
    rvls <- for vls $ \v -> do
       r <- resolvePValueString v
       ismatched <- matchRE regexp r
       return (r, ismatched)
    return $ PArray $ (V.map (PString . fst) (V.filter snd rvls))
_grep [x,_] = throwPosError ("grep(): The first argument must be an Array, not" <+> pretty x)
_grep _ = throwPosError "grep(): Expected two arguments."

isArray :: PValue -> InterpreterMonad PValue
isArray = return . PBoolean . has _PArray

isDomainName :: PValue -> InterpreterMonad PValue
isDomainName s = do
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

isInteger :: PValue -> InterpreterMonad PValue
isInteger = return . PBoolean . has _Integer

isHash :: PValue -> InterpreterMonad PValue
isHash = return . PBoolean . has _PHash

isString :: PValue -> InterpreterMonad PValue
isString pv = return $ PBoolean $ case (pv ^? _PString, pv ^? _Number) of
                                     (_, Just _) -> False
                                     (Just _, _) -> True
                                     _           -> False

keys :: PValue -> InterpreterMonad PValue
keys (PHash h) = return (PArray $ V.fromList $ map PString $ HM.keys h)
keys x = throwPosError ("keys(): Expects a Hash, not" <+> pretty x)

hasKey :: [PValue] -> InterpreterMonad PValue
hasKey [PHash h, k] = do
    k' <- resolvePValueString k
    return (PBoolean (has (ix k') h))
hasKey [a, _] = throwPosError ("has_key(): expected a Hash, not" <+> pretty a)
hasKey _ = throwPosError ("has_key(): expected two arguments.")

merge :: [PValue] -> InterpreterMonad PValue
merge [PHash a, PHash b] = return (PHash (b `HM.union` a))
merge [a,b] = throwPosError ("merge(): Expects two hashes, not" <+> pretty a <+> pretty b)
merge _ = throwPosError "merge(): Expects two hashes"

pick :: [PValue] -> InterpreterMonad PValue
pick [] = throwPosError "pick(): must receive at least one non empty value"
pick (a:as)
    | a `elem` [PUndef, PString "", PString "undef"] = pick as
    | otherwise = return a

size :: PValue -> InterpreterMonad PValue
size (PHash h) = return (_Integer # fromIntegral (HM.size h))
size (PArray v) = return (_Integer # fromIntegral (V.length v))
size x = throwPosError ("size(): Expects a hash, not" <+> pretty x)

str2Bool :: PValue -> InterpreterMonad PValue
str2Bool PUndef = return (PBoolean False)
str2Bool a@(PBoolean _) = return a
str2Bool a = do
    s <- resolvePValueString a
    let b | s `elem` ["", "1", "t", "y", "true", "yes"] = Just True
          | s `elem` [    "0", "f", "n", "false", "no"] = Just False
          | otherwise = Nothing
    case b of
        Just x -> return (PBoolean x)
        Nothing -> throwPosError "str2bool(): Unknown type of boolean given"

validateAbsolutePath :: [PValue] -> InterpreterMonad PValue
validateAbsolutePath [] = throwPosError "validateAbsolutePath(): wrong number of arguments, must be > 0"
validateAbsolutePath a = mapM_ (resolvePValueString >=> validate) a >> return PUndef
    where
        validate x | T.head x == '/' = return ()
                   | otherwise = throwPosError (ttext x <+> "is not an absolute path")

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
    rstr <- resolvePValueString str
    rest <- mapM (resolvePValueString >=> compileRE >=> flip matchRE rstr) (V.toList v)
    if or rest
        then return PUndef
        else throwPosError (pretty msg <$> "Source string:" <+> pretty str <> comma <+> "regexps:" <+> pretty (V.toList v))
validateRe [_, r, _] = throwPosError ("validate_re(): expected a regexp or an array of regexps, but not" <+> pretty r)
validateRe _ = throwPosError "validate_re(): wrong number of arguments (#{args.length}; must be 2 or 3)"

validateString :: [PValue] -> InterpreterMonad PValue
validateString [] = throwPosError "validate_string(): wrong number of arguments, must be > 0"
validateString x = mapM_ resolvePValueString x >> return PUndef

pvalues :: PValue -> InterpreterMonad PValue
pvalues (PHash h) = return $ PArray (toVectorOf traverse h)
pvalues x = throwPosError ("values(): expected a hash, not" <+> pretty x)
