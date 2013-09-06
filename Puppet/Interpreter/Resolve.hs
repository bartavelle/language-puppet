module Puppet.Interpreter.Resolve where

import Puppet.PP
import Puppet.Interpreter.Types
import Puppet.Parser.Types
import Puppet.Interpreter.PrettyPrinter()
import Puppet.Parser.PrettyPrinter()

import Data.Version (parseVersion)
import Text.ParserCombinators.ReadP (readP_to_S)

import Data.Aeson hiding ((.=))
import Data.CaseInsensitive  ( mk )
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import Data.Monoid
import Control.Applicative hiding ((<$>))
import Control.Exception
import Control.Monad
import Control.Monad.Error
import Data.Tuple.Strict
import Control.Lens
import Data.Attoparsec.Number
import Data.Attoparsec.Text
import qualified Data.Either.Strict as S
import qualified Data.Maybe.Strict as S
import Text.Regex.PCRE.ByteString
import Puppet.Interpreter.RubyRandom
import qualified Data.ByteString as BS
import qualified Crypto.Hash.MD5 as MD5
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString.Base16 as B16
import Text.Regex.PCRE.ByteString.Utils
import Data.Bits

type NumberPair = S.Either (Pair Integer Integer) (Pair Double Double)

-- | Tries to convert a pair of PValues into numbers, as defined in
-- attoparsec. If the two values can be converted, it will convert them so
-- that they are of the same type
toNumbers :: PValue -> PValue -> S.Maybe NumberPair
toNumbers (PString a) (PString b) =
    case parseOnly number a :!: parseOnly number b of
        (Right (I x) :!: Right (I y)) -> S.Just (S.Left (x :!: y))
        (Right (D x) :!: Right (D y)) -> S.Just (S.Right (x :!: y))
        (Right (I x) :!: Right (D y)) -> S.Just (S.Right (fromIntegral x :!: y))
        (Right (D x) :!: Right (I y)) -> S.Just (S.Right (x :!: fromIntegral y))
        _ -> S.Nothing
toNumbers _ _ = S.Nothing

binaryOperation :: Expression -> Expression -> (Integer -> Integer -> Integer) -> (Double -> Double -> Double) -> InterpreterMonad PValue
binaryOperation a b opi opd = do
    ra <- resolveExpression a
    rb <- resolveExpression b
    case toNumbers ra rb of
        S.Nothing -> throwPosError ("Expected numbers, not" <+> pretty ra <+> "or" <+> pretty rb)
        S.Just (S.Right (na :!: nb)) -> return (pvnum # D (opd na nb))
        S.Just (S.Left (na :!: nb))  -> return (pvnum # I (opi na nb))

integerOperation :: Expression -> Expression -> (Integer -> Integer -> Integer) -> InterpreterMonad PValue
integerOperation a b opr = do
    ra <- resolveExpression a
    rb <- resolveExpression b
    case toNumbers ra rb of
        S.Nothing -> throwPosError ("Expected numbers, not" <+> pretty ra <+> "or" <+> pretty rb)
        S.Just (S.Right _) -> throwPosError ("Expected integer values, not" <+> pretty ra <+> "or" <+> pretty rb)
        S.Just (S.Left (na :!: nb))  -> return (pvnum # I (opr na nb))

-- | Converting PValue to and from Number with a prism!
pvnum :: Prism PValue PValue Number Number
pvnum = prism num2PValue toNumber
    where
        num2PValue :: Number -> PValue
        num2PValue (I x) = PString (T.pack (show x))
        num2PValue (D x) = PString (T.pack (show x))
        toNumber :: PValue -> Either PValue Number
        toNumber p@(PString x) = case parseOnly number x of
                                     Right y -> Right y
                                     _ -> Left p
        toNumber p = Left p

resolveVariable :: T.Text -> InterpreterMonad PValue
resolveVariable fullvar = do
    scps <- use scopes
    scp <- getScope
    case getVariable scps scp fullvar of
        Left rr -> throwPosError rr
        Right x -> return x

isNativeType :: T.Text -> InterpreterMonad Bool
isNativeType t = view (nativeTypes . contains t)

getVariable :: Container ScopeInformation -> T.Text -> T.Text -> Either Doc PValue
getVariable scps scp fullvar = do
    (varscope, varname) <- case T.splitOn "::" fullvar of
                               [] -> throwError "This doesn't make any sense in resolveVariable"
                               [vn] -> return (scp, vn) -- Non qualified variables
                               rst -> return (T.intercalate "::" (filter (not . T.null) (init rst)), last rst) -- qualified variables
    let extractVariable (varval :!: _) = return varval
    case scps ^? ix varscope . scopeVariables . ix varname of
        Just pp -> extractVariable pp
        Nothing -> -- check top level scope
            case scps ^? ix "::" . scopeVariables . ix varname of
                Just pp -> extractVariable pp
                Nothing -> throwError ("Could not resolve variable" <+> pretty (UVariableReference fullvar) <+> "in context" <+> ttext varscope <+> "or root")

numberCompare :: Expression -> Expression -> (Integer -> Integer -> Bool) -> (Double -> Double -> Bool) -> InterpreterMonad PValue
numberCompare a b compi compd = do
    ra <- resolveExpression a
    rb <- resolveExpression b
    case toNumbers ra rb of
        S.Nothing -> throwPosError ("Comparison functions expect numbers, not:" <+> pretty ra <+> comma <+> pretty rb)
        S.Just (S.Right (na :!: nb)) -> return (PBoolean (compd na nb))
        S.Just (S.Left  (na :!: nb)) -> return (PBoolean (compi na nb))

puppetEquality :: PValue -> PValue -> Bool
puppetEquality ra rb =
    case toNumbers ra rb of
        (S.Just (S.Right (na :!: nb))) -> na == nb
        (S.Just (S.Left (na :!: nb))) -> na == nb
        _ -> case (ra, rb) of
                 (PString "true", PBoolean x) -> x
                 (PString "false", PBoolean x) -> not x
                 (PBoolean x, PString "true") -> x
                 (PBoolean x, PString "false") -> not x
                 (PString sa, PString sb) -> mk sa == mk sb
                 -- TODO, check if array / hash equality should be recursed
                 -- for case insensitive matching
                 _ -> ra == rb

resolveExpression :: Expression -> InterpreterMonad PValue
resolveExpression (PValue v) = resolveValue v
resolveExpression (Not e) = fmap (PBoolean . not . pValue2Bool) (resolveExpression e)
resolveExpression (And a b) = do
    ra <- fmap pValue2Bool (resolveExpression a)
    rb <- fmap pValue2Bool (resolveExpression b)
    return (PBoolean (ra && rb))
resolveExpression (Or a b) = do
    ra <- fmap pValue2Bool (resolveExpression a)
    rb <- fmap pValue2Bool (resolveExpression b)
    return (PBoolean (ra || rb))
resolveExpression (LessThan a b) = numberCompare a b (<) (<)
resolveExpression (MoreThan a b) = numberCompare a b (>) (>)
resolveExpression (LessEqualThan a b) = numberCompare a b (<=) (<=)
resolveExpression (MoreEqualThan a b) = numberCompare a b (>=) (>=)
resolveExpression (RegexMatch a (PValue ur@(URegexp _ rv))) = do
    ra <- fmap T.encodeUtf8 (resolveExpressionString a)
    mtch <- liftIO (execute rv ra)
    case mtch of
        Left rr -> throwPosError ("Regexp matching critical failure" <+> text (show rr) <+> parens ("Regexp was" <+> pretty ur))
        Right Nothing -> return (PBoolean False)
        Right _ -> return (PBoolean True)
resolveExpression (RegexMatch _ t) = throwPosError ("The regexp matching operator expects a regular expression, not" <+> pretty t)
resolveExpression (NotRegexMatch a v) = resolveExpression (Not (RegexMatch a v))
resolveExpression (Equal a b) = do
    ra <- resolveExpression a
    rb <- resolveExpression b
    return $ PBoolean $ puppetEquality ra rb
resolveExpression (Different a b) = resolveExpression (Not (Equal a b))
resolveExpression (Contains idx a) = do
    src <- resolveExpression a
    case src of
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
        _ -> throwPosError ("Can't use the 'in' operator with" <+> pretty src)
resolveExpression (Lookup a idx) = do
    src <- resolveExpression a
    case src of
        PHash h -> do
            ridx <- resolveExpressionString idx
            case h ^. at ridx of
                Just v -> return v
                Nothing -> throwPosError ("Can't find index '" <> ttext ridx <> "' in" <+> pretty src)
        PArray ar -> do
            ridx <- resolveExpression idx
            i <- case ridx ^? pvnum of
                     Just (I n) -> return (fromIntegral n)
                     _ -> throwPosError ("Need an integral number for indexing an array, not" <+> pretty ridx)
            let arl = V.length ar
            if arl <= i
                then throwPosError ("Out of bound indexing, array size is" <+> int arl <+> "index is" <+> int i)
                else return (ar V.! i)
        _ -> throwPosError ("This data can't be indexed:" <+> pretty src)
resolveExpression (ConditionalValue e conds) = do
    rese <- resolveExpression e
    let checkCond [] = throwPosError ("The selector didn't match anything for input" <+> pretty rese)
        checkCond ((SelectorDefault :!: ce) : _) = resolveExpression ce
        checkCond ((SelectorValue ur@(URegexp _ rg) :!: ce) : xs) = do
            rs <- fmap T.encodeUtf8 (resolvePValueString rese)
            mtch <- liftIO (execute rg rs)
            case mtch of
                Left rr -> throwPosError ("Regexp matching critical failure" <+> text (show rr) <+> parens ("Regexp was" <+> pretty ur))
                Right Nothing -> checkCond xs
                Right _ -> resolveExpression ce
        checkCond ((SelectorValue uv :!: ce) : xs) = do
            rv <- resolveValue uv
            if puppetEquality rese rv
                then resolveExpression ce
                else checkCond xs
    checkCond (V.toList conds)
resolveExpression (Addition a b)       = binaryOperation a b (+) (+)
resolveExpression (Substraction a b)   = binaryOperation a b (-) (-)
resolveExpression (Division a b)       = binaryOperation a b div (/)
resolveExpression (Multiplication a b) = binaryOperation a b (*) (*)
resolveExpression (Modulo a b)         = integerOperation a b mod
resolveExpression (RightShift a b)     = integerOperation a b (\x -> shiftR x . fromIntegral)
resolveExpression (LeftShift a b)      = integerOperation a b (\x -> shiftL x . fromIntegral)
resolveExpression x = throwPosError ("Don't know how to resolve this expression:" <$> pretty x)

resolveValue :: UValue -> InterpreterMonad PValue
resolveValue n@(URegexp _ _) = throwPosError ("Regular expressions are not allowed in this context: " <+> pretty n)
resolveValue (UBoolean x) = return (PBoolean x)
resolveValue (UString x) = return (PString x)
resolveValue UUndef = return PUndef
resolveValue (UInterpolable vals) = fmap (PString . mconcat) (mapM resolveValueString (V.toList vals))
resolveValue (UResourceReference t e) = PResourceReference `fmap` pure t <*> resolveExpressionString e
resolveValue (UArray a) = fmap PArray (V.mapM resolveExpression a)
resolveValue (UHash a) = fmap (PHash . HM.fromList) (mapM resPair (V.toList a))
    where
        resPair (k :!: v) = (,) `fmap` resolveExpressionString k <*> resolveExpression v
resolveValue (UVariableReference v) = resolveVariable v
resolveValue (UFunctionCall fname args) = resolveFunction fname args

resolveValueString :: UValue -> InterpreterMonad T.Text
resolveValueString = resolveValue >=> resolvePValueString

resolvePValueString :: PValue -> InterpreterMonad T.Text
resolvePValueString (PString x) = return x
resolvePValueString (PBoolean True) = return "true"
resolvePValueString (PBoolean False) = return "false"
resolvePValueString x = throwPosError ("Don't know how to convert this to a string:" <$> pretty x)

resolveExpressionString :: Expression -> InterpreterMonad T.Text
resolveExpressionString = resolveExpression >=> resolvePValueString

resolveExpressionStrings :: Expression -> InterpreterMonad [T.Text]
resolveExpressionStrings x = do
    rx <- resolveExpression x
    case rx of
        PArray a -> mapM resolvePValueString (V.toList a)
        y -> fmap return (resolvePValueString y)

resolveArgument :: Pair T.Text Expression -> InterpreterMonad (Pair T.Text PValue)
resolveArgument (argname :!: argval) = (:!:) `fmap` pure argname <*> resolveExpression argval

pValue2Bool :: PValue -> Bool
pValue2Bool PUndef = False
pValue2Bool (PString "") = False
pValue2Bool (PBoolean x) = x
pValue2Bool _ = True

resolveFunction :: T.Text -> V.Vector Expression -> InterpreterMonad PValue
resolveFunction "fqdn_rand" args = do
    let nbargs = V.length args
    when (nbargs < 1 || nbargs > 2) (throwPosError "fqdn_rand(): Expects one or two arguments")
    fqdn <- resolveVariable "::fqdn" >>= resolvePValueString
    (mx:targs) <- mapM resolveExpressionString (V.toList args)
    curmax <- case PString mx ^? pvnum of
                  Just (I x) -> return x
                  _ -> throwPosError ("fqdn_rand(): the first argument must be an integer, not" <+> ttext mx)
    let rargs = if null targs
                 then [fqdn, ""]
                 else fqdn : targs
        val = fromIntegral (Prelude.fst (limitedRand (randInit myhash) (fromIntegral curmax)))
        myhash = toint (MD5.hash (T.encodeUtf8 fullstring)) :: Integer
        toint = BS.foldl' (\c nx -> c*256 + fromIntegral nx) 0
        fullstring = T.intercalate ":" rargs
    return (pvnum # I val)
resolveFunction fname args = mapM resolveExpression (V.toList args) >>= resolveFunction' fname

resolveFunction' :: T.Text -> [PValue] -> InterpreterMonad PValue
resolveFunction' "defined" [PResourceReference rt rn] = fmap PBoolean (use (definedResources . contains (RIdentifier rt rn)))
resolveFunction' "defined" [ut] = do
    t <- resolvePValueString ut
    -- case 1, netsted thingie
    nestedStuff <- use nestedDeclarations
    if (nestedStuff ^. contains (TopDefine, t)) || (nestedStuff ^. contains (TopClass, t))
        then return (PBoolean True)
        else do -- case 2, loadeded class
            lc <- use loadedClasses
            if lc ^. contains t
                then return (PBoolean True)
                else fmap PBoolean (isNativeType t)
resolveFunction' "defined" x = throwPosError ("defined(): expects a single resource reference, type or class name, and not" <+> pretty x)
resolveFunction' "fail" x = throwPosError ("fail:" <+> pretty x)
resolveFunction' "inline_template" [templatename] = calcTemplate Left templatename
resolveFunction' "inline_template" _ = throwPosError "inline_template(): Expects a single argument"
resolveFunction' "md5" [pstr] = fmap (PString . T.decodeUtf8 . B16.encode . MD5.hash  . T.encodeUtf8) (resolvePValueString pstr)
resolveFunction' "md5" _ = throwPosError "md5(): Expects a single argument"
resolveFunction' "regsubst" [ptarget, pregexp, preplacement] = resolveFunction' "regsubst" [ptarget, pregexp, preplacement, PString "G"]
resolveFunction' "regsubst" [ptarget, pregexp, preplacement, pflags] = do
    -- TODO handle all the flags
    -- http://docs.puppetlabs.com/references/latest/function.html#regsubst
    when (pflags /= "G") (throwPosError "regsubst(): Currently only supports a single flag (G)")
    target      <- fmap T.encodeUtf8 (resolvePValueString ptarget)
    regexp      <- fmap T.encodeUtf8 (resolvePValueString pregexp)
    replacement <- fmap T.encodeUtf8 (resolvePValueString preplacement)
    r <- liftIO (substituteCompile regexp target replacement)
    case r of
        Left rr -> throwPosError ("regsubst():" <+> text rr)
        Right x -> fmap PString (safeDecodeUtf8 x)
resolveFunction' "regsubst" _ = throwPosError "regsubst(): Expects 3 or 4 arguments"
resolveFunction' "split" [psrc, psplt] = do
    src  <- fmap T.encodeUtf8 (resolvePValueString psrc)
    splt <- fmap T.encodeUtf8 (resolvePValueString psplt)
    r <- liftIO (splitCompile splt src)
    case r of
        Left rr -> throwPosError ("regsubst():" <+> text rr)
        Right x -> fmap (PArray . V.fromList) $ mapM (fmap PString . safeDecodeUtf8) x
resolveFunction' "sha1" [pstr] = fmap (PString . T.decodeUtf8 . B16.encode . SHA1.hash  . T.encodeUtf8) (resolvePValueString pstr)
resolveFunction' "sha1" _ = throwPosError "sha1(): Expects a single argument"
resolveFunction' "mysql_password" [pstr] = fmap (PString . T.decodeUtf8 . B16.encode . SHA1.hash . SHA1.hash  . T.encodeUtf8) (resolvePValueString pstr)
resolveFunction' "mysql_password" _ = throwPosError "mysql_password(): Expects a single argument"
resolveFunction' "file" args = mapM resolvePValueString args >>= fmap PString . interpreterIO . file
    where
        file :: [T.Text] -> IO (S.Either Doc T.Text)
        file [] = return $ S.Left ("No file found in" <+> pretty args)
        file (x:xs) = fmap S.Right (T.readFile (T.unpack x)) `catch` (\SomeException{} -> file xs)
resolveFunction' "tagged" ptags = do
    tags <- fmap HS.fromList (mapM resolvePValueString ptags)
    scp <- getScope
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
resolveFunction' "pdbresourcequery" [q] = pdbresourcequery q Nothing
resolveFunction' "pdbresourcequery" [q,k] = fmap Just (resolvePValueString k) >>= pdbresourcequery q
resolveFunction' "pdbresourcequery" _ = throwPosError "pdbresourcequery(): Expects one or two arguments"
-- user functions
resolveFunction' fname args = do
    external <- view externalFunctions
    case external ^. at fname of
        Just f -> f args
        Nothing -> throwPosError ("Unknown function" <+> dullred (ttext fname))

pdbresourcequery :: PValue -> Maybe T.Text -> InterpreterMonad PValue
pdbresourcequery q key = do
    qf <- view puppetDBquery
    let toDoc (S.Left x) = S.Left (text x)
        toDoc (S.Right x) = S.Right x
    r  <- interpreterIO (fmap toDoc (qf "resources" (toJSON q)))
    rv <- case fromJSON r of
        Error rr -> throwPosError ("Could not parse the PuppetDB response:" <+> text rr)
        Success (PArray a) -> return a
        Success x -> throwPosError ("Expected an array from the PuppetDB, but received" <+> pretty x)
    let extractSubHash :: T.Text -> PValue -> InterpreterMonad PValue
        extractSubHash ky (PHash h) = case h ^. at ky of
                                         Just val -> return val
                                         Nothing -> throwPosError ("pdbresourcequery strange error, could not find key" <+> ttext ky <+> "in" <+> pretty (PHash h))
        extractSubHash _ x = throwPosError ("pdbresourcequery strange error, expected a hash, had" <+> pretty x)
    case key of
        Nothing -> return (PArray rv)
        (Just k) -> fmap PArray (V.mapM (extractSubHash k) rv)

calcTemplate :: (T.Text -> Either T.Text T.Text) -> PValue -> InterpreterMonad PValue
calcTemplate templatetype templatename = do
    fname       <- resolvePValueString templatename
    scps        <- use scopes
    scp         <- getScope
    computeFunc <- view computeTemplateFunction
    res         <- liftIO (computeFunc (templatetype fname) scp scps)
    case res of
        S.Left rr -> throwPosError ("template error for" <+> ttext fname <+> ":" <$> rr)
        S.Right r -> return (PString r)

resolveExpressionSE :: Expression -> InterpreterMonad PValue
resolveExpressionSE e = resolveExpression e >>= \resolved ->
    case resolved of
        PArray _ -> throwPosError "The use of an array in a search expression is undefined"
        PHash _ -> throwPosError "The use of an array in a search expression is undefined"
        _ -> return resolved

resolveSearchExpression :: SearchExpression -> InterpreterMonad RSearchExpression
resolveSearchExpression AlwaysTrue = return RAlwaysTrue
resolveSearchExpression (EqualitySearch a e) = REqualitySearch `fmap` pure a <*> resolveExpressionSE e
resolveSearchExpression (NonEqualitySearch a e) = RNonEqualitySearch `fmap` pure a <*> resolveExpressionSE e
resolveSearchExpression (AndSearch e1 e2) = RAndSearch `fmap` resolveSearchExpression e1 <*> resolveSearchExpression e2
resolveSearchExpression (OrSearch e1 e2) = ROrSearch `fmap` resolveSearchExpression e1 <*> resolveSearchExpression e2

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
