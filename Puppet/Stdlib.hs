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
stdlibFunctions = HM.fromList [ ("abs", puppet_abs)
                              , ("any2array", any2array)
                              , ("base64", base64)
                              , ("bool2num", bool2num)
                              , ("capitalize", stringArrayFunction (safeEmptyString (\t -> T.cons (toUpper (T.head t)) (T.tail t))))
                              , ("chomp", stringArrayFunction (T.dropWhileEnd (\c -> c == '\n' || c == '\r')))
                              , ("chop", stringArrayFunction (safeEmptyString T.init))
                              , ("concat", puppetConcat)
                              , ("count", puppetCount)
                              , ("downcase", stringArrayFunction T.toLower)
                              , ("is_domain_name", is_domain_name)
                              , ("is_integer", is_integer)
                              , ("lstrip", stringArrayFunction T.stripStart)
                              , ("rstrip", stringArrayFunction T.stripEnd)
                              , ("strip", stringArrayFunction T.strip)
                              , ("upcase", stringArrayFunction T.toUpper)
                              , ("validate_array", validate_array)
                              , ("validate_bool", validate_bool)
                              , ("validate_hash", validate_hash)
                              , ("validate_re", validate_re)
                              , ("validate_string", validate_string)
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
compileRE p = do
    er <- liftIO . compile compBlank execBlank . T.encodeUtf8 $ p
    case er of
        Right r -> return r
        Left ms -> throwPosError ("Can't parse regexp" <+> pretty (URegexp p undefined) <+> ":" <+> text (show ms))

puppet_abs :: [PValue] -> InterpreterMonad PValue
puppet_abs [y] = case y ^? pvnum of
                     Just (I x) -> return $ pvnum # I (abs x)
                     Just (D x) -> return $ pvnum # D (abs x)
                     Nothing -> throwPosError ("abs(): Expects a number, not" <+> pretty y)
puppet_abs _ = throwPosError "abs(): Takes a single argument"

any2array :: [PValue] -> InterpreterMonad PValue
any2array [PArray v] = return (PArray v)
any2array [PHash h] = return (PArray lst)
    where lst = V.fromList $ concatMap arraypair $ HM.toList h
          arraypair (a,b) = [PString a, b]
any2array [x] = return (PArray (V.singleton x))
any2array x = return (PArray (V.fromList x))

base64 :: [PValue] -> InterpreterMonad PValue
base64 [pa,pb] = do
    a <- resolvePValueString pa
    b <- fmap T.encodeUtf8 (resolvePValueString pb)
    r <- case a of
        "encode" -> return (B16.encode b)
        "decode" -> case B16.decode b of
                        (x, "") -> return x
                        _       -> throwPosError ("base64(): could not decode" <+> pretty pb)
        _        -> throwPosError ("base64(): the first argument must be either 'encode' or 'decode', not" <+> ttext a)
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

is_domain_name :: [PValue] -> InterpreterMonad PValue
is_domain_name [s] = do
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
is_domain_name _ = throwPosError "is_domain_name(): Should only take a single argument"

is_integer :: [PValue] -> InterpreterMonad PValue
is_integer [i] = return (PBoolean (not (isn't pvnum i)))
is_integer _ = throwPosError "is_integer(): Should only take a single argument"

validate_array :: [PValue] -> InterpreterMonad PValue
validate_array [] = throwPosError "validate_array(): wrong number of arguments, must be > 0"
validate_array x = mapM_ vb x >> return PUndef
    where
        vb (PArray _) = return ()
        vb y = throwPosError (pretty y <+> "is not an array.")

validate_bool :: [PValue] -> InterpreterMonad PValue
validate_bool [] = throwPosError "validate_bool(): wrong number of arguments, must be > 0"
validate_bool x = mapM_ vb x >> return PUndef
    where
        vb (PBoolean _) = return ()
        vb y = throwPosError (pretty y <+> "is not a boolean.")

validate_hash :: [PValue] -> InterpreterMonad PValue
validate_hash [] = throwPosError "validate_hash(): wrong number of arguments, must be > 0"
validate_hash x = mapM_ vb x >> return PUndef
    where
        vb (PHash _) = return ()
        vb y = throwPosError (pretty y <+> "is not a hash.")

validate_re :: [PValue] -> InterpreterMonad PValue
validate_re [str, reg] = validate_re [str, reg, PString "Match failed"]
validate_re [str, PString reg, msg] = validate_re [str, PArray (V.singleton (PString reg)), msg]
validate_re [str, PArray v, msg] = do
    rstr <- fmap T.encodeUtf8 (resolvePValueString str)
    let matchRE :: Regex -> InterpreterMonad Bool
        matchRE r = liftIO (execute r rstr) >>= \res -> case res of
                        Left rr -> throwPosError ("Regexp matching critical failure" <+> text (show rr))
                        Right Nothing -> return False
                        _ -> return True
    rest <- mapM (resolvePValueString >=> compileRE >=> matchRE) (V.toList v)
    if or rest
        then return PUndef
        else throwPosError (pretty msg <$> "Source string:" <+> pretty str <> comma <+> "regexps:" <+> pretty (V.toList v))
validate_re [_, r, _] = throwPosError ("validate_re(): expected a regexp or an array of regexps, but not" <+> pretty r)
validate_re _ = throwPosError "validate_re(): wrong number of arguments (#{args.length}; must be 2 or 3)"

validate_string :: [PValue] -> InterpreterMonad PValue
validate_string [] = throwPosError "validate_string(): wrong number of arguments, must be > 0"
validate_string x = mapM_ resolvePValueString x >> return PUndef
