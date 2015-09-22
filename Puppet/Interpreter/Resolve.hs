{-# LANGUAGE LambdaCase #-}
-- | This module is all about converting and resolving foreign data into
-- the fully exploitable corresponding data type. The main use case is the
-- conversion of 'Expression' to 'PValue'.
module Puppet.Interpreter.Resolve
    ( -- * Pure resolution functions
      getVariable,
      pValue2Bool,
      -- * Monadic resolution functions
      resolveVariable,
      resolveExpression,
      resolveValue,
      resolvePValueString,
      resolveExpressionString,
      resolveExpressionStrings,
      resolveArgument,
      resolveFunction',
      runHiera,
      isNativeType,
      -- * Search expression management
      resolveSearchExpression,
      checkSearchExpression,
      searchExpressionToPuppetDB,
      -- * Higher order puppet functions handling
      hfGenerateAssociations,
      hfSetvars,
      hfRestorevars,
      toNumbers,
      fixResourceName
    ) where

import           Puppet.Interpreter.PrettyPrinter ()
import           Puppet.Interpreter.RubyRandom
import           Puppet.Interpreter.Types
import           Puppet.Parser.Types
import           Puppet.Pathes
import           Puppet.PP
import           Puppet.Utils

import           Control.Lens
import           Control.Monad
import           Control.Monad.Operational        (singleton)
import           Crypto.Hash
import           Data.Aeson                       hiding ((.=))
import           Data.Aeson.Lens                  hiding (key)
import           Data.Bits
import           Data.ByteString (ByteString)
import           Data.ByteArray (convert)
import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Base16           as B16
import           Data.CaseInsensitive             (mk)
import qualified Data.HashMap.Strict              as HM
import qualified Data.HashSet                     as HS
import           Data.Maybe                       (mapMaybe,fromMaybe)
import qualified Data.Maybe.Strict                as S
import           Data.Scientific
import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as T
import           Data.Tuple.Strict                as S
import qualified Data.Vector                      as V
import           Data.Version                     (parseVersion)
import           Text.ParserCombinators.ReadP     (readP_to_S)
import qualified Text.PrettyPrint.ANSI.Leijen     as PP
import           Text.Regex.PCRE.ByteString.Utils

sha1 :: ByteString -> ByteString
sha1 = convert . (hash :: ByteString -> Digest SHA1)

md5 :: ByteString -> ByteString
md5 = convert . (hash :: ByteString -> Digest MD5)

-- | A useful type that is used when trying to perform arithmetic on Puppet
-- numbers.
type NumberPair = Pair Scientific Scientific

-- | Converts class resource names to lowercase (fix for the jenkins
-- plugin).
fixResourceName :: T.Text -- ^ Resource type
                -> T.Text -- ^ Resource name
                -> T.Text
fixResourceName "class" x = T.toLower $ fromMaybe x $ T.stripPrefix "::" x
fixResourceName _       x = x

-- | A hiera helper function, that will throw all Hiera errors and log
-- messages to the main monad.
runHiera :: T.Text -> HieraQueryType -> InterpreterMonad (Maybe PValue)
runHiera q t = do
    -- We need to merge the current scope with the top level scope
    scps <- use scopes
    ctx  <- getScopeName
    let getV scp = mapMaybe toStr $ HM.toList $ fmap (view (_1 . _1)) (scps ^. ix scp . scopeVariables)
        -- we can't use _PString, because of dependency cycles
        toStr (k,v) = case v of
                          PString x -> Just (k,x)
                          _ -> Nothing
        toplevels = map (_1 %~ ("::" <>)) $ getV "::"
        locals = getV ctx
        vars = HM.fromList (toplevels <> locals)
    singleton (HieraQuery vars q t)

-- | The implementation of all hiera_* functions
hieraCall :: HieraQueryType -> PValue -> Maybe PValue -> Maybe PValue -> InterpreterMonad PValue
hieraCall _ _ _ (Just _) = throwPosError "Overriding the hierarchy is not yet supported"
hieraCall qt q df _ = do
    qs <- resolvePValueString q
    o <- runHiera qs qt
    case o of
        Just p  -> return p
        Nothing -> case df of
                         Just d -> return d
                         Nothing -> throwPosError ("Lookup for " <> ttext qs <> " failed")

-- | Tries to convert a pair of 'PValue's into 'Number's, as defined in
-- attoparsec. If the two values can be converted, it will convert them so
-- that they are of the same type
toNumbers :: PValue -> PValue -> S.Maybe NumberPair
toNumbers (PString a) b = case text2Scientific a of
                              Just na -> toNumbers (PNumber na) b
                              Nothing -> S.Nothing
toNumbers a (PString b) = toNumbers (PString b) a
toNumbers (PNumber a) (PNumber b) = S.Just (a :!: b)
toNumbers _ _ = S.Nothing

-- | This tries to run a numerical binary operation on two puppet
-- expressions. It will try to resolve them, then convert them to numbers
-- (using 'toNumbners'), and will finally apply the correct operation.
binaryOperation :: Expression -- ^ left operand
                -> Expression -- ^ right operand
                -> (Scientific -> Scientific -> Scientific) -- ^ operation
                -> InterpreterMonad PValue
binaryOperation a b opr = ((PNumber .) . opr) `fmap` resolveExpressionNumber a <*> resolveExpressionNumber b

-- Just like 'binaryOperation', but for operations that only work on integers.
integerOperation :: Expression -> Expression -> (Integer -> Integer -> Integer) -> InterpreterMonad PValue
integerOperation a b opr = do
    ra <- resolveExpressionNumber a
    rb <- resolveExpressionNumber b
    case (preview _Integer ra, preview _Integer rb) of
        (Just na, Just nb) -> return (PNumber $ fromIntegral (opr na nb))
        _ -> throwPosError ("Expected integer values, not" <+> string (show ra) <+> "or" <+> string (show rb))

-- | Resolves a variable, or throws an error if it can't.
resolveVariable :: T.Text -> InterpreterMonad PValue
resolveVariable fullvar = do
    scps <- use scopes
    scp <- getScopeName
    case getVariable scps scp fullvar of
        Left rr -> throwPosError rr
        Right x -> return x

-- | A simple helper that checks if a given type is native or a define.
isNativeType :: T.Text -> InterpreterMonad Bool
isNativeType t = has (ix t) `fmap` singleton GetNativeTypes

-- | A pure function for resolving variables.
getVariable :: Container ScopeInformation -- ^ The whole scope data.
            -> T.Text -- ^ Current scope name.
            -> T.Text -- ^ Full variable name.
            -> Either Doc PValue
getVariable scps scp fullvar = do
    (varscope, varname) <- case T.splitOn "::" fullvar of
                               [] -> Left "This doesn't make any sense in resolveVariable"
                               [vn] -> return (scp, vn) -- Non qualified variables
                               rst -> return (T.intercalate "::" (filter (not . T.null) (init rst)), last rst) -- qualified variables
    let extractVariable (varval :!: _ :!: _) = return varval
    case scps ^? ix varscope . scopeVariables . ix varname of
        Just pp -> extractVariable pp
        Nothing -> -- check top level scope
            case scps ^? ix "::" . scopeVariables . ix varname of
                Just pp -> extractVariable pp
                Nothing -> Left ("Could not resolve variable" <+> pretty (UVariableReference fullvar) <+> "in context" <+> ttext varscope <+> "or root")

-- | A helper for numerical comparison functions.
numberCompare :: Expression -> Expression -> (Scientific -> Scientific -> Bool) -> InterpreterMonad PValue
numberCompare a b comp = ((PBoolean .) . comp) `fmap` resolveExpressionNumber a <*> resolveExpressionNumber b

-- | Handles the wonders of puppet equality checks.
puppetEquality :: PValue -> PValue -> Bool
puppetEquality ra rb =
    case toNumbers ra rb of
        (S.Just (na :!: nb)) -> na == nb
        _ -> case (ra, rb) of
                 (PUndef , PBoolean x)         -> not x
                 (PString "true", PBoolean x)  -> x
                 (PString "false", PBoolean x) -> not x
                 (PBoolean x, PString "true")  -> x
                 (PBoolean x, PString "false") -> not x
                 (PString sa, PString sb)      -> mk sa == mk sb
                 -- TODO, check if array / hash equality should be recursed
                 -- for case insensitive matching
                 _ -> ra == rb

-- | The main resolution function : turns an 'Expression' into a 'PValue',
-- if possible.
resolveExpression :: Expression -> InterpreterMonad PValue
resolveExpression (Terminal v) = resolveValue v
resolveExpression (Not e) = fmap (PBoolean . not . pValue2Bool) (resolveExpression e)
resolveExpression (And a b) = do
    ra <- fmap pValue2Bool (resolveExpression a)
    if ra
        then do
            rb <- fmap pValue2Bool (resolveExpression b)
            return (PBoolean (ra && rb))
        else return (PBoolean False)
resolveExpression (Or a b) = do
    ra <- fmap pValue2Bool (resolveExpression a)
    if ra
        then return (PBoolean True)
        else do
            rb <- fmap pValue2Bool (resolveExpression b)
            return (PBoolean (ra || rb))
resolveExpression (LessThan a b) = numberCompare a b (<)
resolveExpression (MoreThan a b) = numberCompare a b (>)
resolveExpression (LessEqualThan a b) = numberCompare a b (<=)
resolveExpression (MoreEqualThan a b) = numberCompare a b (>=)
resolveExpression (RegexMatch a v@(Terminal (URegexp (CompRegex _ rv)))) = do
    ra <- fmap T.encodeUtf8 (resolveExpressionString a)
    case execute' rv ra of
        Left (_,rr)    -> throwPosError ("Error when evaluating" <+> pretty v <+> ":" <+> string rr)
        Right Nothing  -> return $ PBoolean False
        Right (Just _) -> return $ PBoolean True
resolveExpression (RegexMatch _ t) = throwPosError ("The regexp matching operator expects a regular expression, not" <+> pretty t)
resolveExpression (NotRegexMatch a v) = resolveExpression (Not (RegexMatch a v))
resolveExpression (Equal a b) = do
    ra <- resolveExpression a
    rb <- resolveExpression b
    return $ PBoolean $ puppetEquality ra rb
resolveExpression (Different a b) = resolveExpression (Not (Equal a b))
resolveExpression (Contains idx a) =
    resolveExpression a >>= \case
        PHash h -> do
            ridx <- resolveExpressionString idx
            case h ^. at ridx of
                Just _ -> return (PBoolean True)
                Nothing -> return (PBoolean False)
        PArray ar -> do
            ridx <- resolveExpression idx
            return (PBoolean (ridx `V.elem` ar))
        PString st -> do
            ridx <- resolveExpressionString idx
            return (PBoolean (ridx `T.isInfixOf` st))
        src -> throwPosError ("Can't use the 'in' operator with" <+> pretty src)
resolveExpression (Lookup a idx) =
    resolveExpression a >>= \case
        PHash h -> do
            ridx <- resolveExpressionString idx
            case h ^. at ridx of
                Just v -> return v
                Nothing -> do
                  checkStrict
                    ("Look up for an hash with the unknown key '" <> ttext ridx <> "' for" <+> pretty (PHash h))
                    ("Can't find index '" <> ttext ridx <> "' in" <+> pretty (PHash h))
                  return "undef"
        PArray ar -> do
            ridx <- resolveExpression idx
            i <- case ridx ^? _Integer of
                     Just n -> return (fromIntegral n)
                     _ -> throwPosError ("Need an integral number for indexing an array, not" <+> pretty ridx)
            let arl = V.length ar
            if arl <= i
                then throwPosError ("Out of bound indexing, array size is" <+> int arl <+> "index is" <+> int i)
                else return (ar V.! i)
        src -> throwPosError ("This data can't be indexed:" <+> pretty src)
resolveExpression stmt@(ConditionalValue e conds) = do
    rese <- resolveExpression e
    let checkCond [] = throwPosError ("The selector didn't match anything for input" <+> pretty rese </> pretty stmt)
        checkCond ((SelectorDefault :!: ce) : _) = resolveExpression ce
        checkCond ((SelectorValue v@(URegexp (CompRegex _ rg)) :!: ce) : xs) = do
            rs <- fmap T.encodeUtf8 (resolvePValueString rese)
            case execute' rg rs of
                Left (_,rr)    -> throwPosError ("Could not match" <+> pretty v <+> ":" <+> string rr)
                Right Nothing  -> checkCond xs
                Right (Just _) -> resolveExpression ce
        checkCond ((SelectorValue uv :!: ce) : xs) = do
            rv <- resolveValue uv
            if puppetEquality rese rv
                then resolveExpression ce
                else checkCond xs
    checkCond (V.toList conds)
resolveExpression (Addition a b) = do
    ra <- resolveExpression a
    rb <- resolveExpression b
    case (ra, rb) of
        (PHash ha, PHash hb) -> return (PHash (ha <> hb))
        (PArray ha, PArray hb) -> return (PArray (ha <> hb))
        _ -> binaryOperation a b (+)
resolveExpression (Substraction a b)   = binaryOperation a b (-)
resolveExpression (Division a b)       = do
    ra <- resolveExpressionNumber a
    rb <- resolveExpressionNumber b
    case rb of
        0 -> throwPosError "Division by 0"
        _ -> case (,) `fmap` preview _Integer ra <*> preview _Integer rb of
                 Just (ia, ib) -> return $ PNumber $ fromIntegral (ia `div` ib)
                 _ -> return $ PNumber $ ra / rb
resolveExpression (Multiplication a b) = binaryOperation a b (*)
resolveExpression (Modulo a b)         = integerOperation a b mod
resolveExpression (RightShift a b)     = integerOperation a b (\x -> shiftR x . fromIntegral)
resolveExpression (LeftShift a b) = do
    ra <- resolveExpression a
    rb <- resolveExpression b
    case (ra, rb) of
        (PArray ha, v) -> return (PArray (V.snoc ha v))
        _ -> integerOperation a b (\x -> shiftL x . fromIntegral)
resolveExpression a@(FunctionApplication e (Terminal (UHFunctionCall hf))) = do
    unless (S.isNothing (hf ^. hfexpr)) (throwPosError ("You can't combine chains of higher order functions (with .) and giving them parameters, in:" <+> pretty a))
    resolveValue (UHFunctionCall (hf & hfexpr .~ S.Just e))
resolveExpression (FunctionApplication _ x) = throwPosError ("Expected function application here, not" <+> pretty x)
resolveExpression (Negate x) = PNumber . negate <$> resolveExpressionNumber x

-- | Resolves an 'UValue' (terminal for the 'Expression' data type) into
-- a 'PValue'
resolveValue :: UValue -> InterpreterMonad PValue
resolveValue (UNumber n) = return (PNumber n)
resolveValue n@(URegexp _) = throwPosError ("Regular expressions are not allowed in this context: " <+> pretty n)
resolveValue (UBoolean x) = return (PBoolean x)
resolveValue (UString x) = return (PString x)
resolveValue UUndef = return PUndef
resolveValue (UInterpolable vals) = fmap (PString . mconcat) (mapM resolveExpressionString (V.toList vals))
resolveValue (UResourceReference t e) = do
    r <- resolveExpressionStrings e
    case r of
        [s] -> return (PResourceReference t (fixResourceName t s))
        _   -> return (PArray (V.fromList (map (PResourceReference t . fixResourceName t) r)))
resolveValue (UArray a) = fmap PArray (V.mapM resolveExpression a)
resolveValue (UHash a) = fmap (PHash . HM.fromList) (mapM resPair (V.toList a))
    where
        resPair (k :!: v) = (,) `fmap` resolveExpressionString k <*> resolveExpression v
resolveValue (UVariableReference v) = resolveVariable v
resolveValue (UFunctionCall fname args) = resolveFunction fname args
resolveValue (UHFunctionCall hf) = evaluateHFCPure hf

-- | Turns strings, numbers and booleans into 'T.Text', or throws an error.
resolvePValueString :: PValue -> InterpreterMonad T.Text
resolvePValueString (PString x) = return x
resolvePValueString (PBoolean True) = return "true"
resolvePValueString (PBoolean False) = return "false"
resolvePValueString (PNumber x) = return (scientific2text x)
resolvePValueString PUndef = do
     checkStrict
       "Resolving the keyword `undef` to the string \"undef\""
       "Strict mode won't convert the keyword `undef` to the string \"undef\""
     return "undef"
resolvePValueString x = throwPosError ("Don't know how to convert this to a string:" PP.<$> pretty x)

-- | Turns everything it can into a number, or throws an error
resolvePValueNumber :: PValue -> InterpreterMonad Scientific
resolvePValueNumber x = case x ^? _Number of
                            Just n -> return n
                            Nothing -> throwPosError ("Don't know how to convert this to a number:" PP.<$> pretty x)

-- | > resolveExpressionString = resolveExpression >=> resolvePValueString
resolveExpressionString :: Expression -> InterpreterMonad T.Text
resolveExpressionString = resolveExpression >=> resolvePValueString

-- | > resolveExpressionNumber = resolveExpression >=> resolvePValueNumber
resolveExpressionNumber :: Expression -> InterpreterMonad Scientific
resolveExpressionNumber = resolveExpression >=> resolvePValueNumber

-- | Just like 'resolveExpressionString', but accepts arrays.
resolveExpressionStrings :: Expression -> InterpreterMonad [T.Text]
resolveExpressionStrings x =
    resolveExpression x >>= \case
        PArray a -> mapM resolvePValueString (V.toList a)
        y -> fmap return (resolvePValueString y)

-- | A special helper function for argument like argument like pairs.
resolveArgument :: Pair T.Text Expression -> InterpreterMonad (Pair T.Text PValue)
resolveArgument (argname :!: argval) = (:!:) `fmap` pure argname <*> resolveExpression argval

-- | Turns a 'PValue' into a 'Bool', as explained in the reference
-- documentation.
pValue2Bool :: PValue -> Bool
pValue2Bool PUndef = False
pValue2Bool (PString "") = False
pValue2Bool (PBoolean x) = x
pValue2Bool _ = True

-- | This resolve function calls at the expression level.
resolveFunction :: T.Text -> V.Vector Expression -> InterpreterMonad PValue
resolveFunction "fqdn_rand" args = do
    let nbargs = V.length args
    when (nbargs < 1 || nbargs > 2) (throwPosError "fqdn_rand(): Expects one or two arguments")
    fqdn <- resolveVariable "::fqdn" >>= resolvePValueString
    (mx:targs) <- mapM resolveExpressionString (V.toList args)
    curmax <- case PString mx ^? _Integer of
                  Just x -> return x
                  _ -> throwPosError ("fqdn_rand(): the first argument must be an integer, not" <+> ttext mx)
    let rargs = if null targs
                 then [fqdn, ""]
                 else fqdn : targs
        val = fromIntegral (Prelude.fst (limitedRand (randInit myhash) (fromIntegral curmax)))
        myhash = toint (md5 (T.encodeUtf8 fullstring)) :: Integer
        toint = BS.foldl' (\c nx -> c*256 + fromIntegral nx) 0
        fullstring = T.intercalate ":" rargs
    return (_Integer # val)
resolveFunction fname args = mapM resolveExpression (V.toList args) >>= resolveFunction' fname . map undefEmptyString
    where
        undefEmptyString PUndef = PString ""
        undefEmptyString x = x

resolveFunction' :: T.Text -> [PValue] -> InterpreterMonad PValue
resolveFunction' "defined" [PResourceReference "class" cn] = do
    checkStrict "The use of the 'defined' function is a code smell"
                "The 'defined' function is not allowed in strict mode."
    fmap (PBoolean . has (ix cn)) (use loadedClasses)
resolveFunction' "defined" [PResourceReference rt rn] = do
    checkStrict "The use of the 'defined' function is a code smell"
                "The 'defined' function is not allowed in strict mode."
    fmap (PBoolean . has (ix (RIdentifier rt rn))) (use definedResources)
resolveFunction' "defined" [ut] = do
    checkStrict "The use of the 'defined' function is a code smell."
                "The 'defined' function is not allowed in strict mode."
    t <- resolvePValueString ut
    -- case 1, netsted thingie
    nestedStuff <- use nestedDeclarations
    if has (ix (TopDefine, t)) nestedStuff || has (ix (TopClass, t)) nestedStuff
        then return (PBoolean True)
        else do -- case 2, loadeded class
            lc <- use loadedClasses
            if has (ix t) lc
                then return (PBoolean True)
                else fmap PBoolean (isNativeType t)
resolveFunction' "defined" x = throwPosError ("defined(): expects a single resource reference, type or class name, and not" <+> pretty x)
resolveFunction' "fail" x = throwPosError ("fail:" <+> pretty x)
resolveFunction' "inline_template" [templatename] = calcTemplate Left templatename
resolveFunction' "inline_template" _ = throwPosError "inline_template(): Expects a single argument"
resolveFunction' "md5" [pstr] = fmap (PString . T.decodeUtf8 . B16.encode . md5 . T.encodeUtf8) (resolvePValueString pstr)
resolveFunction' "md5" _ = throwPosError "md5(): Expects a single argument"
resolveFunction' "regsubst" [ptarget, pregexp, preplacement] = resolveFunction' "regsubst" [ptarget, pregexp, preplacement, PString "G"]
resolveFunction' "regsubst" [ptarget, pregexp, preplacement, pflags] = do
    -- TODO handle all the flags
    -- http://docs.puppetlabs.com/references/latest/function.html#regsubst
    when (pflags /= "G") (use curPos >>= \p -> warn ("regsubst(): Currently only supports a single flag (G) " <> showPos (S.fst p)))
    regexp      <- fmap T.encodeUtf8 (resolvePValueString pregexp)
    replacement <- fmap T.encodeUtf8 (resolvePValueString preplacement)
    let sub t = do
            t' <- fmap T.encodeUtf8 (resolvePValueString t)
            case substituteCompile' regexp t' replacement of
                    Left rr -> throwPosError ("regsubst():" <+> string rr)
                    Right x -> fmap PString (safeDecodeUtf8 x)
    case ptarget of
        PArray a -> fmap PArray (traverse sub a)
        s -> sub s
resolveFunction' "regsubst" _ = throwPosError "regsubst(): Expects 3 or 4 arguments"
resolveFunction' "split" [psrc, psplt] = do
    src  <- fmap T.encodeUtf8 (resolvePValueString psrc)
    splt <- fmap T.encodeUtf8 (resolvePValueString psplt)
    case splitCompile' splt src of
        Left rr -> throwPosError ("splitCompile():" <+> string rr)
        Right x -> fmap (PArray . V.fromList) (mapM (fmap PString . safeDecodeUtf8) x)
resolveFunction' "sha1" [pstr] = fmap (PString . T.decodeUtf8 . B16.encode . sha1 . T.encodeUtf8) (resolvePValueString pstr)
resolveFunction' "sha1" _ = throwPosError "sha1(): Expects a single argument"
resolveFunction' "mysql_password" [pstr] = fmap (PString . T.decodeUtf8 . B16.encode . sha1 . sha1  . T.encodeUtf8) (resolvePValueString pstr)
resolveFunction' "mysql_password" _ = throwPosError "mysql_password(): Expects a single argument"
resolveFunction' "file" args = mapM (resolvePValueString >=> fixFilePath) args >>= fmap PString . singleton . ReadFile
    where
        fixFilePath s | T.null s = let rr = "Empty file path passed to the 'file' function" in checkStrict rr rr >> return s
                      | T.head s == '/' = return s
                      | otherwise = case T.splitOn "/" s of
                                        (md:x:rst) -> do
                                            moduledir <- view modulesPath <$> getPuppetPathes
                                            return (T.intercalate "/" (T.pack moduledir : md : "files" : x : rst))
                                        _ -> throwPosError ("file() argument invalid: " <> ttext s)
resolveFunction' "tagged" ptags = do
    tags <- fmap HS.fromList (mapM resolvePValueString ptags)
    scp <- getScopeName
    scpset <- use (scopes . ix scp . scopeExtraTags)
    return (PBoolean (scpset `HS.intersection` tags == tags))
resolveFunction' "template" [templatename] = calcTemplate Right templatename
resolveFunction' "template" _ = throwPosError "template(): Expects a single argument"
resolveFunction' "versioncmp" [pa,pb] = do
    a <- resolvePValueString pa
    b <- resolvePValueString pb
    let parser x = case filter (null . Prelude.snd) (readP_to_S parseVersion (T.unpack x)) of
                       ( (v, _) : _ ) -> return v
                       _ -> throwPosError ("Could not parse this string as a version:" <+> ttext x)
    va <- parser a
    vb <- parser b
    return $ PString $ case compare va vb of
                           EQ -> "0"
                           LT -> "-1"
                           GT -> "1"
resolveFunction' "versioncmp" _ = throwPosError "versioncmp(): Expects two arguments"
-- some custom functions
resolveFunction' "pdbresourcequery" [q]   = pdbresourcequery q Nothing
resolveFunction' "pdbresourcequery" [q,k] = fmap Just (resolvePValueString k) >>= pdbresourcequery q
resolveFunction' "pdbresourcequery" _     = throwPosError "pdbresourcequery(): Expects one or two arguments"
resolveFunction' "hiera"       [q]     = hieraCall Priority   q Nothing  Nothing
resolveFunction' "hiera"       [q,d]   = hieraCall Priority   q (Just d) Nothing
resolveFunction' "hiera"       [q,d,o] = hieraCall Priority   q (Just d) (Just o)
resolveFunction' "hiera_array" [q]     = hieraCall ArrayMerge q Nothing  Nothing
resolveFunction' "hiera_array" [q,d]   = hieraCall ArrayMerge q (Just d) Nothing
resolveFunction' "hiera_array" [q,d,o] = hieraCall ArrayMerge q (Just d) (Just o)
resolveFunction' "hiera_hash"  [q]     = hieraCall HashMerge  q Nothing  Nothing
resolveFunction' "hiera_hash"  [q,d]   = hieraCall HashMerge  q (Just d) Nothing
resolveFunction' "hiera_hash"  [q,d,o] = hieraCall HashMerge  q (Just d) (Just o)
resolveFunction' "hiera" _ = throwPosError "hiera(): Expects one, two or three arguments"

-- user functions
resolveFunction' fname args = singleton (ExternalFunction fname args)

pdbresourcequery :: PValue -> Maybe T.Text -> InterpreterMonad PValue
pdbresourcequery q mkey = do
    rrv <- case fromJSON (toJSON q) of
               Success rq -> singleton (PDBGetResources rq)
               Error rr   -> throwPosError ("Invalid resource query:" <+> Puppet.PP.string rr)
    rv <- case fromJSON (toJSON rrv) of
              Success x -> return x
              Error rr -> throwPosError ("For some reason we could not convert a resource list to Puppet internal values!!" <+> Puppet.PP.string rr <+> pretty rrv)
    let extractSubHash :: T.Text -> PValue -> InterpreterMonad PValue
        extractSubHash ky (PHash h) = case h ^. at ky of
                                         Just val -> return val
                                         Nothing -> throwPosError ("pdbresourcequery strange error, could not find key" <+> ttext ky <+> "in" <+> pretty (PHash h))
        extractSubHash _ x = throwPosError ("pdbresourcequery strange error, expected a hash, had" <+> pretty x)
    case mkey of
        Nothing -> return (PArray rv)
        (Just k) -> fmap PArray (V.mapM (extractSubHash k) rv)

calcTemplate :: (T.Text -> Either T.Text T.Text) -> PValue -> InterpreterMonad PValue
calcTemplate templatetype templatename = do
    fname       <- resolvePValueString templatename
    stt         <- use id
    PString `fmap` singleton (ComputeTemplate (templatetype fname) stt)

resolveExpressionSE :: Expression -> InterpreterMonad PValue
resolveExpressionSE e = resolveExpression e >>=
    \case
        PArray _ -> throwPosError "The use of an array in a search expression is undefined"
        PHash _  -> throwPosError "The use of an array in a search expression is undefined"
        resolved -> return resolved

-- | Turns an unresolved 'SearchExpression' from the parser into a fully
-- resolved 'RSearchExpression'.
resolveSearchExpression :: SearchExpression -> InterpreterMonad RSearchExpression
resolveSearchExpression AlwaysTrue = return RAlwaysTrue
resolveSearchExpression (EqualitySearch a e) = REqualitySearch `fmap` pure a <*> resolveExpressionSE e
resolveSearchExpression (NonEqualitySearch a e) = RNonEqualitySearch `fmap` pure a <*> resolveExpressionSE e
resolveSearchExpression (AndSearch e1 e2) = RAndSearch `fmap` resolveSearchExpression e1 <*> resolveSearchExpression e2
resolveSearchExpression (OrSearch e1 e2) = ROrSearch `fmap` resolveSearchExpression e1 <*> resolveSearchExpression e2

-- | Turns a resource type and 'RSearchExpression' into something that can
-- be used in a PuppetDB query.
searchExpressionToPuppetDB :: T.Text -> RSearchExpression -> Query ResourceField
searchExpressionToPuppetDB rtype res = QAnd ( QEqual RType (capitalizeRT rtype) : mkSE res )
    where
        mkSE (RAndSearch a b) = [QAnd (mkSE a ++ mkSE b)]
        mkSE (ROrSearch a b) = [QOr (mkSE a ++ mkSE b)]
        mkSE (RNonEqualitySearch a b) = fmap QNot (mkSE (REqualitySearch a b))
        mkSE (REqualitySearch a (PString b)) = [QEqual (mkFld a) b]
        mkSE _ = []
        mkFld "tag" = RTag
        mkFld "title" = RTitle
        mkFld z = RParameter z

-- | Checks whether a given 'Resource' matches a 'RSearchExpression'. Note
-- that the expression doesn't check for type, so you must filter the
-- resources by type beforehand, if needs be.
checkSearchExpression :: RSearchExpression -> Resource -> Bool
checkSearchExpression RAlwaysTrue _ = True
checkSearchExpression (RAndSearch a b) r = checkSearchExpression a r && checkSearchExpression b r
checkSearchExpression (ROrSearch a b) r = checkSearchExpression a r || checkSearchExpression b r
checkSearchExpression (RNonEqualitySearch a b) r = not (checkSearchExpression (REqualitySearch a b) r)
checkSearchExpression (REqualitySearch "tag" (PString s)) r = r ^. rtags . contains s
checkSearchExpression (REqualitySearch "tag" _) _ = False
checkSearchExpression (REqualitySearch "title" v) r =
    let nameequal = puppetEquality v (PString (r ^. rid . iname))
        aliasequal = case r ^. rattributes . at "alias" of
                         Just a -> puppetEquality v a
                         Nothing -> False
    in nameequal || aliasequal
checkSearchExpression (REqualitySearch attributename v) r = case r ^. rattributes . at attributename of
                                                                Nothing -> False
                                                                Just x -> puppetEquality x v

-- | Generates variable associations for evaluation of blocks. Each item
-- corresponds to an iteration in the calling block.
hfGenerateAssociations :: HFunctionCall -> InterpreterMonad [[(T.Text, PValue)]]
hfGenerateAssociations hf = do
    sourceexpression <- case hf ^. hfexpr of
                            S.Just x -> return x
                            S.Nothing -> throwPosError ("No expression to run the function on" <+> pretty hf)
    sourcevalue <- resolveExpression sourceexpression
    case (sourcevalue, hf ^. hfparams) of
         (PArray pr, BPSingle varname) -> return (map (\x -> [(varname, x)]) (V.toList pr))
         (PArray pr, BPPair idx var) -> return $ do
             (i,v) <- Prelude.zip ([0..] :: [Int]) (V.toList pr)
             return [(idx,PString (T.pack (show i))),(var,v)]
         (PHash hh, BPSingle varname) -> return $ do
             (k,v) <- HM.toList hh
             return [(varname, PArray (V.fromList [PString k,v]))]
         (PHash hh, BPPair idx var) -> return $ do
             (k,v) <- HM.toList hh
             return [(idx,PString k),(var,v)]
         (invalid, _) -> throwPosError ("Can't iterate on this data type:" <+> pretty invalid)

-- | Sets the proper variables, and returns the scope variables the way
-- they were before being modified. This is a hack that ensures that
-- variables are local to the new scope.
--
-- It doesn't work at all like other Puppet parts, but consistency isn't
-- really expected here ...
hfSetvars :: [(T.Text, PValue)] -> InterpreterMonad (Container (Pair (Pair PValue PPosition) CurContainerDesc))
hfSetvars vals =
    do
        scp <- getScopeName
        p <- use curPos
        container <- getCurContainer
        save <- use (scopes . ix scp . scopeVariables)
        let hfSetvar (varname, varval) = scopes . ix scp . scopeVariables . at varname ?= (varval :!: p :!: (container ^. cctype))
        mapM_ hfSetvar vals
        return save

-- | Restores what needs restoring. This will erase all allocations.
hfRestorevars :: Container (Pair (Pair PValue PPosition) CurContainerDesc) -> InterpreterMonad ()
hfRestorevars save =
    do
        scp <- getScopeName
        scopes . ix scp . scopeVariables .= save

-- | Evaluates a statement in "pure" mode. TODO
evalPureStatement :: Statement -> InterpreterMonad ()
evalPureStatement _ = throwPosError "So called 'pure' statements are not yet supported"

-- | This extracts the final expression from an HFunctionCall.
-- When it does not exists, it checks if the last statement is in fact
-- a function call
transformPureHf :: HFunctionCall -> InterpreterMonad (HFunctionCall, Expression)
transformPureHf hf =
        case hf ^. hfexpression of
            S.Just x -> return (hf, x)
            S.Nothing -> do
                let statements = hf ^. hfstatements
                if V.null statements
                    then throwPosError ("The statement block must not be empty" <+> pretty hf)
                    else case V.last statements of
                             (MainFunctionCall (MFC fn args _)) ->
                                let expr = Terminal (UFunctionCall fn args)
                                in  return (hf & hfstatements %~ V.init
                                               & hfexpression .~ S.Just expr
                                           , expr)
                             _ -> throwPosError ("The statement block must end with an expression" <+> pretty hf)

-- | All the "higher order function" stuff, for "value" mode. In this case
-- we are in "pure" mode, and only a few statements are allowed.
evaluateHFCPure :: HFunctionCall -> InterpreterMonad PValue
evaluateHFCPure hf' = do
    (hf, finalexpression) <- transformPureHf hf'
    varassocs <- hfGenerateAssociations hf
    let runblock :: [(T.Text, PValue)] -> InterpreterMonad PValue
        runblock assocs = do
            saved <- hfSetvars assocs
            V.mapM_ evalPureStatement (hf ^. hfstatements)
            r <- resolveExpression finalexpression
            hfRestorevars  saved
            return r
    case hf ^. hftype of
        HFEach -> throwPosError "The 'each' function can't be used at the value level in language-puppet. Please use map."
        HFMap -> fmap (PArray . V.fromList) (mapM runblock varassocs)
        HFFilter -> do
            res <- mapM (fmap pValue2Bool . runblock) varassocs
            sourcevalue <- case hf ^. hfexpr of
                               S.Just x -> resolveExpression x
                               S.Nothing -> throwPosError "Internal error evaluateHFCPure 1"
            case sourcevalue of
                PArray ar -> return $ PArray             $ V.map Prelude.fst $ V.filter Prelude.snd       $ V.zip ar             (V.fromList res)
                PHash  hh -> return $ PHash  $ HM.fromList $ map Prelude.fst   $ filter Prelude.snd $ Prelude.zip (HM.toList hh) res
                x -> throwPosError ("Can't iterate on this data type:" <+> pretty x)
        x -> throwPosError ("This type of function is not supported yet by language-puppet!" <+> pretty x)
