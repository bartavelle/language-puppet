{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TupleSections #-}
module Puppet.Runner.Stdlib (stdlibFunctions) where

import           XPrelude                         hiding (sort)

import           Data.Aeson.Lens
import qualified Data.ByteString.Base16           as B16
import qualified Data.Char                        as Char
import qualified Data.HashMap.Strict              as HM
import qualified Data.List                        as List
import qualified Data.List.Split                  as List (chunksOf)
import qualified Data.Scientific                  as Scientific
import qualified Data.Text                        as Text
import           Data.Text.Lens                   (unpacked)
import qualified Data.Vector                      as V
import           Data.Vector.Lens                 (toVectorOf)
import qualified Text.Regex.PCRE.ByteString.Utils as Regex
import qualified System.FilePath                  as FilePath

import           Puppet.Interpreter

-- | Contains the implementation of the StdLib functions.
stdlibFunctions :: Container ( [PValue] -> InterpreterMonad PValue )
stdlibFunctions = HM.fromList [ singleArgument "abs" puppetAbs
                              , ("any2array", any2array)
                              , ("assert_private", assertPrivate)
                              , ("base64", base64)
                              -- basename
                              , singleArgument "bool2num" bool2num
                              -- bool2str
                              -- camelcase
                              , ("capitalize", stringArrayFunction (safeEmptyString (\t -> Text.cons (Char.toUpper (Text.head t)) (Text.tail t))))
                              -- ceiling
                              , ("chomp", stringArrayFunction (Text.dropWhileEnd (\c -> c == '\n' || c == '\r')))
                              , ("chop", stringArrayFunction (safeEmptyString Text.init))
                              -- clamp
                              , ("concat", puppetConcat)
                              -- convert_base
                              , ("count", puppetCount)
                              -- deep_merge
                              , ("defined_with_params", const (throwPosError "defined_with_params can't be implemented with language-puppet"))
                              , ("delete", delete)
                              , ("delete_at", deleteAt)
                              , singleArgument "delete_undef_values" deleteUndefValues
                              -- delete_values
                              -- difference
                              , singleArgument "dirname" dirname
                              -- dos2unix
                              , ("downcase", stringArrayFunction Text.toLower)
                              , singleArgument "empty" _empty
                              -- ensure_packages (in main interpreter module)
                              -- ensure_resource (in main interpreter module)
                              , singleArgument "fact" fact
                              , singleArgument "flatten" flatten
                              -- floor
                              -- fqdn_rand_string
                              -- fqdn_rotate
                              -- get_module_path
                              , ("getparam", const $ throwPosError "The getparam function is uncool and shall not be implemented in language-puppet")
                              , singleArgument "getvar"  getvar
                              , ("grep", _grep)
                              , ("hash", hash)
                              -- has_interface_with
                              -- has_ip_address
                              -- has_ip_network
                              , ("has_key", hasKey)
                              -- intersection
                              -- is_absolute_path
                              , singleArgument "is_array" isArray
                              , singleArgument "is_bool" isBool
                              , singleArgument "is_domain_name" isDomainName
                              -- is_float
                              -- is_function_available
                              , singleArgument "is_hash" isHash
                              , singleArgument "is_integer" isInteger
                              -- is_ip_address
                              -- is_mac_address
                              -- is_numeric
                              , singleArgument "is_string" isString
                              , ("join", puppetJoin)
                              , ("join_keys_to_values", joinKeysToValues)
                              , singleArgument "keys" keys
                              , singleArgument "length" size
                              -- load_module_metadata
                              -- loadyaml
                              , ("lstrip", stringArrayFunction Text.stripStart)
                              -- max
                              , ("member", member)
                              , ("merge", merge)
                              -- min
                              -- num2bool
                              -- parsejson
                              -- parseyaml
                              , ("pick", pick)
                              , ("pick_default", pickDefault)
                              , ("prefix", prefix)
                              -- private
                              , ("pw_hash", pwHash)
                              -- range
                              -- reject
                              -- reverse
                              , ("rstrip", stringArrayFunction Text.stripEnd)
                              -- seeded_rand
                              -- shuffle
                              , singleArgument "size" size
                              , singleArgument "sort" sort
                              -- squeeze
                              , singleArgument "str2bool" str2Bool
                              -- strtosaltedshar512
                              -- strftime
                              , ("strip", stringArrayFunction Text.strip)
                              , ("suffix", suffix )
                              -- swapcase
                              -- time
                              -- to_bytes
                              -- try_get_value
                              -- type3x
                              -- type
                              -- union
                              , singleArgument "unique" unique
                              -- from puppetlabs-translate
                              , ("translate", translate)
                              -- unix2dos
                              , ("upcase", stringArrayFunction Text.toUpper)
                              -- uriescape
                              , ("validate_absolute_path", validateAbsolutePath)
                              , ("validate_array", validateArray)
                              -- validate_augeas
                              , ("validate_bool", validateBool)
                              -- validate_cmd
                              , ("validate_hash", validateHash)
                              , ("validate_integer", validateInteger)
                              -- validate_ip_address
                              -- validate_ipv4_address
                              -- validate_ipv6_address
                              , ("validate_numeric", validateNumeric)
                              , ("validate_re", validateRe)
                              -- validate_slength
                              , ("validate_string", validateString)
                              -- validate_x509_rsa_key_pair
                              -- values_at
                              , singleArgument "values" pvalues
                              -- zip
                              ]

singleArgument :: Text -> (PValue -> InterpreterMonad PValue) -> (Text, [PValue] -> InterpreterMonad PValue )
singleArgument fname ifunc = (fname, ofunc)
  where
    ofunc [x] = ifunc x
    ofunc _   = throwPosError (ppline fname <> "(): Expects a single argument.")

safeEmptyString :: (Text -> Text) -> Text -> Text
safeEmptyString _ "" = ""
safeEmptyString f x  = f x

stringArrayFunction :: (Text -> Text) -> [PValue] -> InterpreterMonad PValue
stringArrayFunction f [PString s] = return (PString (f s))
stringArrayFunction f [PArray xs] = fmap PArray (V.mapM (fmap (PString . f) . resolvePValueString) xs)
stringArrayFunction _ [a] = throwPosError ("function expects a string or an array of strings, not" <+> pretty a)
stringArrayFunction _ _ = throwPosError "function expects a single argument"

compileRE :: Text -> InterpreterMonad Regex
compileRE r =
  case Regex.compile' Regex.compBlank Regex.execBlank (encodeUtf8 r) of
    Left rr -> throwPosError ("Could not compile" <+> ppline r <+> ":" <+> ppline (show rr))
    Right x -> return x

matchRE :: Regex -> Text -> InterpreterMonad Bool
matchRE r t =
  case Regex.execute' r (encodeUtf8 t) of
    Left rr -> throwPosError ("Could not match:" <+> pplines (show rr))
    Right m -> return (has _Just m)

puppetAbs :: PValue -> InterpreterMonad PValue
puppetAbs y = case y ^? _Number of
                  Just x  -> return $ _Number # abs x
                  Nothing -> throwPosError ("abs(): Expects a number, not" <+> pretty y)

suffix :: [PValue] -> InterpreterMonad PValue
suffix = foofix "suffix" (flip (<>))

prefix :: [PValue] -> InterpreterMonad PValue
prefix = foofix "prefix" (<>)

-- Dummy mock implementation of pw_hash
-- To be implemented if required
pwHash :: [PValue] -> InterpreterMonad PValue
pwHash [PString pwd, PString algo, PString salt] =
  pure (PString ("plain " <> pwd <> "(crypt with " <> algo <> " and " <> salt))
pwHash _ = throwPosError "pw_hash(): expects 3 string arguments"

foofix :: Doc -> (Text -> Text -> Text) -> [PValue] -> InterpreterMonad PValue
foofix nm f args =
    case args of
      [PHash h] -> pure (PHash h)
      [PArray r] -> pure (PArray r)
      [_] -> throwPosError (nm <> ": expects the first argument to be an array or a hash")
      [PHash h, PString s] -> pure (PHash . HM.fromList . map (_1 %~ f s) . HM.toList $ h)
      [PArray r, PString s] -> pure (PArray (r & traverse . _PString %~ f s))
      [PHash _, _] -> throwPosError (nm <> ": expects the second argument to be a string")
      [PArray _, _] -> throwPosError (nm <> ": expects the second argument to be a string")
      [_, _] -> throwPosError (nm <> ": expects the first argument to be an array or a hash")
      _ -> throwPosError (nm <> ": expects two arguments")

assertPrivate :: [PValue] -> InterpreterMonad PValue
assertPrivate args =
  case args of
    []   -> go Nothing
    [pv] -> resolvePValueString pv >>= go . Just
    _    -> throwPosError "assert_private: expects no or a single string argument"
  where
    go :: Maybe Text -> InterpreterMonad PValue
    go msg = do
      scp <- use curScope
      case scp of
        funScope:callerScope:_ ->
          let takeModule = Text.takeWhile (/= ':') . containerModName
          in if takeModule funScope == takeModule callerScope
               then return PUndef
               else throwPosError $ maybe ("assert_private: failed: " <> pretty funScope <> " is private") ppline msg
        _ -> return PUndef

any2array :: [PValue] -> InterpreterMonad PValue
any2array [PArray v] = return (PArray v)
any2array [PHash h] = return (PArray lst)
  where
    lst = V.fromList $ concatMap arraypair $ HM.toList h
    arraypair (a,b) = [PString a, b]
any2array [x] = return (PArray (V.singleton x))
any2array x = return (PArray (V.fromList x))

base64 :: [PValue] -> InterpreterMonad PValue
base64 [pa,pb] = do
  b <- encodeUtf8 <$> resolvePValueString pb
  r <- resolvePValueString pa >>= \case
        "encode" -> return (B16.encode b)
        "decode" -> case B16.decode b of
                      (x, "") -> return x
                      _       -> throwPosError ("base64(): could not decode" <+> pretty pb)
        a        -> throwPosError ("base64(): the first argument must be either 'encode' or 'decode', not" <+> ppline a)
  pure $ PString (decodeUtf8 r)
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
puppetConcat = return . PArray . V.concat . map toArr
  where
    toArr (PArray x) = x
    toArr x          = V.singleton x

puppetCount :: [PValue] -> InterpreterMonad PValue
puppetCount [PArray x] = return (_Integer # V.foldl' cnt 0 x)
  where
    cnt cur (PString "") = cur
    cnt cur PUndef       = cur
    cnt cur _            = cur + 1
puppetCount [PArray x, y] = return (_Integer # V.foldl' cnt 0 x)
  where
    cnt cur z | y == z = cur + 1
              | otherwise = cur
puppetCount _ = throwPosError "count(): expects 1 or 2 arguments"

delete :: [PValue] -> InterpreterMonad PValue
delete [PString x, y] = fmap (PString . Text.concat . (`Text.splitOn` x)) (resolvePValueString y)
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
    in  if V.length r <= n
          then throwPosError ("delete_at(): Out of bounds access detected, tried to remove index" <+> pretty z <+> "wheras the array only has" <+> pplines (show lr) <+> "elements")
          else return (PArray (s1 <> s2))
  _ -> throwPosError ("delete_at(): The second argument must be an integer, not" <+> pretty z)
deleteAt [x,_] = throwPosError ("delete_at(): expects its first argument to be an array, not" <+> pretty x)
deleteAt _ = throwPosError "delete_at(): expects 2 arguments"

deleteUndefValues :: PValue -> InterpreterMonad PValue
deleteUndefValues (PArray r) = return $ PArray $ V.filter (/= PUndef) r
deleteUndefValues (PHash h) = return $ PHash $ HM.filter (/= PUndef) h
deleteUndefValues x =  throwPosError ("delete_undef_values(): Expects an Array or a Hash, not" <+> pretty x)

_empty :: PValue -> InterpreterMonad PValue
_empty = return . PBoolean . flip elem [PUndef, PString "", PString "undef", PArray V.empty, PHash HM.empty]

fact :: PValue -> InterpreterMonad PValue
fact (PString k) = do
  let ks = Text.splitOn "." k
      unwrap x = x >>= \case
        Just r -> pure r
        Nothing -> throwPosError ("fact(): Failed to retrieve fact" <+> ppline k)
      found = asum <$> mapM askFact ks
  unwrap found
fact x = throwPosError ("fact(): Expects a String, not" <+> pretty x)

flatten :: PValue -> InterpreterMonad PValue
flatten r@(PArray _) = return $ PArray (flatten' r)
  where
    flatten' :: PValue -> V.Vector PValue
    flatten' (PArray x) = V.concatMap flatten' x
    flatten' x          = V.singleton x
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
  return $ PArray $ V.map (PString . fst) (V.filter snd rvls)
_grep [x,_] = throwPosError ("grep(): The first argument must be an Array, not" <+> pretty x)
_grep _ = throwPosError "grep(): Expected two arguments."

hash :: [PValue] -> InterpreterMonad PValue
hash [PArray elems] = do
  let xs = mapMaybe assocPairs $ List.chunksOf 2 $ V.toList elems
      assocPairs [a,b] = Just (a,b)
      assocPairs _     = Nothing
  PHash . HM.fromList <$> mapM (\(k,v) -> (,v) <$> resolvePValueString k) xs
hash _ = throwPosError "hash(): Expected and array."

isArray :: PValue -> InterpreterMonad PValue
isArray = return . PBoolean . has _PArray

isDomainName :: PValue -> InterpreterMonad PValue
isDomainName s = do
  rs <- resolvePValueString s
  let ndrs = if Text.last rs == '.'
               then Text.init rs
               else rs
      prts = Text.splitOn "." ndrs
      checkPart x = not (Text.null x)
                      && (Text.length x <= 63)
                      && (Text.head x /= '-')
                      && (Text.last x /= '-')
                      && Text.all (\y -> Char.isAlphaNum y || y == '-') x
  return $ PBoolean $ not (Text.null rs) && Text.length rs <= 255 && all checkPart prts

isInteger :: PValue -> InterpreterMonad PValue
isInteger = return . PBoolean . has _Integer

isHash :: PValue -> InterpreterMonad PValue
isHash = return . PBoolean . has _PHash

isString :: PValue -> InterpreterMonad PValue
isString pv = return $ PBoolean $ case (pv ^? _PString, pv ^? _Number) of
                                     (_, Just _) -> False
                                     (Just _, _) -> True
                                     _           -> False

isBool :: PValue -> InterpreterMonad PValue
isBool = return . PBoolean . has _PBoolean

puppetJoin :: [PValue] -> InterpreterMonad PValue
puppetJoin [PArray rr, PString interc] = do
    rrt <- mapM resolvePValueString (V.toList rr)
    return (PString (Text.intercalate interc rrt))
puppetJoin [_,_] = throwPosError "join(): expected an array of strings, and a string"
puppetJoin _ = throwPosError "join(): expected two arguments"

joinKeysToValues :: [PValue] -> InterpreterMonad PValue
joinKeysToValues [PHash h, separator] = do
    ssep <- resolvePValueString separator
    fmap (PArray . V.fromList) $ forM (itoList h) $ \(k,v) -> do
        sv <- case v of
                  PUndef -> return ""
                  _      -> resolvePValueString v
        return  (PString (k <> ssep <> sv))
joinKeysToValues _ = throwPosError "join_keys_to_values(): expects 2 arguments, an hash and a string"

keys :: PValue -> InterpreterMonad PValue
keys (PHash h) = return (PArray $ V.fromList $ map PString $ HM.keys h)
keys x         = throwPosError ("keys(): Expects a Hash, not" <+> pretty x)

member :: [PValue] -> InterpreterMonad PValue
member [PArray v, x] = return $ PBoolean (x `V.elem` v)
member _             = throwPosError "member() expects 2 arguments"

hasKey :: [PValue] -> InterpreterMonad PValue
hasKey [PHash h, k] = do
    k' <- resolvePValueString k
    return (PBoolean (has (ix k') h))
hasKey [a, _] = throwPosError ("has_key(): expected a Hash, not" <+> pretty a)
hasKey _ = throwPosError "has_key(): expected two arguments."

merge :: [PValue] -> InterpreterMonad PValue
merge xs | length xs < 2 = throwPosError "merge(): Expects at least two hashes"
         | otherwise = let hashcontents = mapM (preview _PHash) xs
                       in  case hashcontents of
                               Nothing     -> throwPosError "merge(): Expects hashes"
                               Just hashes -> return $ PHash (getDual $ foldMap Dual hashes)

pick :: [PValue] -> InterpreterMonad PValue
pick [] = throwPosError "pick(): must receive at least one non empty value"
pick xs =
  case filter (`notElem` [PUndef, PString "", PString "undef"]) xs of
    []    -> throwPosError "pick(): no value suitable to be picked"
    (x:_) -> return x

pickDefault :: [PValue] -> InterpreterMonad PValue
pickDefault [] = throwPosError "pick_default(): must receive at least one non empty value"
pickDefault xs =
  case filter (`notElem` [PUndef, PString "", PString "undef"]) xs of
    []    -> return (List.last xs)
    (x:_) -> return x

size :: PValue -> InterpreterMonad PValue
size (PHash h) = return (_Integer # fromIntegral (HM.size h))
size (PArray v) = return (_Integer # fromIntegral (V.length v))
size (PString s) = return (_Integer # fromIntegral (Text.length s))
size x = throwPosError ("size(): Expects a hash, and array or a string, not" <+> pretty x)

unique :: PValue -> InterpreterMonad PValue
unique (PString s) = return $ PString (s & unpacked %~ List.nub)
unique (PArray v) = return $ PArray (V.fromList (List.nub (V.toList v))) -- :(
unique x = throwPosError ("unique(): Expects an array or a string, not" <+> pretty x)

dirname :: PValue -> InterpreterMonad PValue
dirname (PString s) = pure $ PString (s & unpacked %~ FilePath.takeDirectory)
dirname x = throwPosError ("dirname(): Expects a string, not" <+> pretty x)

sort :: PValue -> InterpreterMonad PValue
sort (PArray s) =
  let lst = V.toList s
      msort :: Ord a => Prism' PValue a -> Maybe PValue
      msort prsm = PArray . V.fromList . map (review prsm) . List.sort <$> mapM (preview prsm) lst
  in  case msort _PString <|> msort _PNumber of
        Just x -> return x
        _ -> throwPosError "sort(): only homogeneous arrays of numbers or strings are allowed"
sort x = throwPosError ("sort(): Expect to sort an array, not" <+> pretty x)

str2Bool :: PValue -> InterpreterMonad PValue
str2Bool PUndef = return (PBoolean False)
str2Bool a@(PBoolean _) = return a
str2Bool a = do
  s <- resolvePValueString a
  let b | s `elem` ["", "1", "t", "y", "true", "yes"] = Just True
        | s `elem` [    "0", "f", "n", "false", "no"] = Just False
        | otherwise = Nothing
  case b of
    Just x  -> return (PBoolean x)
    Nothing -> throwPosError "str2bool(): Unknown type of boolean given"

validateAbsolutePath :: [PValue] -> InterpreterMonad PValue
validateAbsolutePath [] = throwPosError "validateAbsolutePath(): wrong number of arguments, must be > 0"
validateAbsolutePath a = mapM_ (resolvePValueString >=> validate) a >> return PUndef
  where
    validate x | Text.head x == '/' = return ()
               | otherwise = throwPosError (ppline x <+> "is not an absolute path")

validateArray :: [PValue] -> InterpreterMonad PValue
validateArray [] = throwPosError "validate_array(): wrong number of arguments, must be > 0"
validateArray x = mapM_ vb x >> return PUndef
  where
    vb (PArray _) = return ()
    vb y          = throwPosError (pretty y <+> "is not an array.")

validateBool :: [PValue] -> InterpreterMonad PValue
validateBool [] = throwPosError "validate_bool(): wrong number of arguments, must be > 0"
validateBool x = mapM_ vb x >> return PUndef
  where
    vb (PBoolean _) = return ()
    vb y            = throwPosError (pretty y <+> "is not a boolean.")

validateHash :: [PValue] -> InterpreterMonad PValue
validateHash [] = throwPosError "validate_hash(): wrong number of arguments, must be > 0"
validateHash x = mapM_ vb x >> return PUndef
  where
    vb (PHash _) = return ()
    vb y         = throwPosError (pretty y <+> "is not a hash.")

validateNumeric :: [PValue] -> InterpreterMonad PValue
validateNumeric [] = throwPosError "validate_numeric: invalid arguments"
validateNumeric (arr:extra) = do
  (mn, mx) <- case extra of
                  [mx'] -> (Nothing,) . Just <$> resolvePValueNumber mx'
                  [PUndef, mi'] -> (,Nothing) . Just <$> resolvePValueNumber mi'
                  [mx',mi'] -> (,) <$> (Just <$> resolvePValueNumber mi') <*> (Just <$> resolvePValueNumber mx')
                  [] -> pure (Nothing, Nothing)
                  _ -> throwPosError "validate_numeric: invalid arguments"
  numbers <- case arr of
      PArray lst -> mapM resolvePValueNumber (V.toList lst)
      _          -> pure <$> resolvePValueNumber arr
  forM_ mn $ \mn' -> unless (all (>= mn') numbers) $ throwPosError "validate_numeric: failure"
  forM_ mx $ \mx' -> unless (all (<= mx') numbers) $ throwPosError "validate_numeric: failure"
  return PUndef

validateRe :: [PValue] -> InterpreterMonad PValue
validateRe [str, reg] = validateRe [str, reg, PString "Match failed"]
validateRe [str, PString reg, msg] = validateRe [str, PArray (V.singleton (PString reg)), msg]
validateRe [str, PArray v, msg] = do
  rstr <- resolvePValueString str
  rest <- mapM (resolvePValueString >=> compileRE >=> flip matchRE rstr) (V.toList v)
  if or rest
    then return PUndef
    else throwPosError (pretty msg <> line <> "Source string:" <+> pretty str <> comma <+> "regexps:" <+> pretty (V.toList v))
validateRe [_, r, _] = throwPosError ("validate_re(): expected a regexp or an array of regexps, but not" <+> pretty r)
validateRe _ = throwPosError "validate_re(): wrong number of arguments (#{args.length}; must be 2 or 3)"

validateString :: [PValue] -> InterpreterMonad PValue
validateString [] = throwPosError "validate_string(): wrong number of arguments, must be > 0"
validateString x  = mapM_ resolvePValueString x >> return PUndef

validateInteger :: [PValue] -> InterpreterMonad PValue
validateInteger [] = throwPosError "validate_integer(): wrong number of arguments, must be > 0"
validateInteger x = PUndef <$ mapM_ vb x
  where
    msg d = pretty d <+> "is not an integer."
    check n = unless (Scientific.isInteger n) $ throwPosError (msg n)
    vb (PNumber n) = check n
    vb (PString s) | Just n <- text2Scientific s = check n
    vb a           = throwPosError (msg a)

pvalues :: PValue -> InterpreterMonad PValue
pvalues (PHash h) = return $ PArray (toVectorOf traverse h)
pvalues x         = throwPosError ("values(): expected a hash, not" <+> pretty x)

-- dummy translate method from puppetlabs-translate (used in puppetlabs-docker for instance)
translate :: [PValue] -> InterpreterMonad PValue
translate [v@(PString _)] = pure v
translate x = throwPosError ("values(): expected a String, not" <+> pretty x)
