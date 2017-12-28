{-# LANGUAGE PackageImports   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TupleSections    #-}

{-# LANGUAGE FlexibleContexts #-}

-- | This module is all about converting and resolving foreign data into
-- the fully exploitable corresponding data type.
--
-- The main use case is the conversion of 'Expression' to 'PValue'.
module Puppet.Interpreter.Resolve
    ( -- * Pure resolution functions
      getVariable,
      pValue2Bool,
      -- * Monadic resolution functions
      resolveVariable,
      resolveExpression,
      resolveValue,
      resolvePValueString,
      resolvePValueNumber,
      resolveExpressionString,
      resolveExpressionStrings,
      resolveFunction',
      resolveDataType,
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
      fixResourceName,
      datatypeMatch,
    ) where

import           XPrelude.Extra
import           XPrelude.PP

import qualified Control.Monad.Operational          as Operational
import           "cryptonite" Crypto.Hash
import qualified Data.Aeson                         as Aeson
import           Data.Aeson.Lens                    (_Integer, _Number)
import qualified Data.ByteArray                     as ByteArray
import qualified Data.ByteString                    as BS
import qualified Data.ByteString.Base16             as B16
import qualified Data.CaseInsensitive               as CaseInsensitive
import qualified Data.Char                          as Char
import qualified Data.HashMap.Strict                as HM
import qualified Data.HashSet                       as HS
import qualified Data.List                          as List
import qualified Data.List.NonEmpty                 as NE
import qualified Data.Maybe.Strict                  as S
import qualified Data.Scientific                    as Scientific
import qualified Data.Text                          as Text
import qualified Data.Text.Encoding                 as Text
import qualified Data.Tuple.Strict                  as Tuple
import qualified Data.Vector                        as V
import           Data.Version                       (Version (..), parseVersion)
import           Text.ParserCombinators.ReadP       (readP_to_S)
import qualified Text.Regex.PCRE.ByteString.Utils   as Regex

import           Hiera.Server
import           Puppet.Interpreter.Helpers
import           Puppet.Interpreter.PrettyPrinter   ()
import           Puppet.Interpreter.Resolve.Sprintf (sprintf)
import           Puppet.Interpreter.RubyRandom
import           Puppet.Interpreter.Types
import           Puppet.Parser
import           PuppetDB

sha1 :: ByteString -> ByteString
sha1 = ByteArray.convert . (hash :: ByteString -> Digest SHA1)

md5 :: ByteString -> ByteString
md5 = ByteArray.convert . (hash :: ByteString -> Digest MD5)

-- | A useful type that is used when trying to perform arithmetic on Puppet numbers.
type NumberPair = Pair Scientific Scientific

-- | Converts class resource names to lowercase (fix for the jenkins plugin).
fixResourceName :: Text -- ^ Resource type
                -> Text -- ^ Resource name
                -> Text
fixResourceName "class" x = Text.toLower $ fromMaybe x $ Text.stripPrefix "::" x
fixResourceName _       x = x

-- | A hiera helper function, that will throw all Hiera errors and log
-- messages to the main monad.
runHiera :: Text -> HieraQueryType -> InterpreterMonad (Maybe PValue)
runHiera q t = do
  -- We need to merge the current scope with the top level scope
  scps <- use scopes
  ctx  <- getScopeName
  let getV scp = mapMaybe toStr $ HM.toList $ fmap (view (_1 . _1)) (scps ^. ix scp . scopeVariables)
      toStr (k,v) = fmap (k,) (preview _PString v)
      toplevels = map (_1 %~ ("::" <>)) $ getV "::"
      locals = getV ctx
      vars = HM.fromList (toplevels <> locals)
  Operational.singleton (HieraQuery vars q t)

-- | The implementation of all hiera_* functions
hieraCall :: HieraQueryType -> PValue -> Maybe PValue -> Maybe DataType -> Maybe PValue -> InterpreterMonad PValue
hieraCall _ _ _ _ (Just _) = throwPosError "Overriding the hierarchy is not supported (and deprecated in puppet)"
hieraCall qt q df dt _ = do
  qs <- resolvePValueString q
  runHiera qs qt >>= \case
    Just p  -> case dt of
      Just dt' | not (datatypeMatch dt' p) -> throwPosError "Datatype mismatched"
      _        -> pure p
    Nothing -> case df of
      Just d  -> pure d
      Nothing -> throwPosError ("Lookup for " <> ppline qs <> " failed")

-- | Tries to convert a pair of 'PValue's into a 'NumberPair', as defined in
-- attoparsec. If the two values can be converted, it will convert them so
-- that they are of the same type
toNumbers :: PValue -> PValue -> S.Maybe NumberPair
toNumbers (PString a) b =
  case text2Scientific a of
    Just na -> toNumbers (PNumber na) b
    Nothing -> S.Nothing
toNumbers a (PString b) = toNumbers (PString b) a
toNumbers (PNumber a) (PNumber b) = S.Just (a :!: b)
toNumbers _ _ = S.Nothing

-- | This tries to run a numerical binary operation on two puppet
-- expressions. It will try to resolve them, then convert them to numbers
-- (using 'toNumbers'), and will finally apply the correct operation.
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
    (Just na, Just nb) -> pure (PNumber $ fromIntegral (opr na nb))
    _ -> throwPosError ("Expected integer values, not" <+> pretty ra <+> "or" <+> pretty rb)

-- | Resolves a variable, or throws an error if it can't.
resolveVariable :: Text -> InterpreterMonad PValue
resolveVariable fullvar = do
  scps <- use scopes
  scp <- getScopeName
  case getVariable scps scp fullvar of
    Left rr -> throwPosError rr
    Right x -> pure x

-- | A simple helper that checks if a given type is native or a define.
isNativeType :: Text -> InterpreterMonad Bool
isNativeType t = has (ix t) `fmap` Operational.singleton GetNativeTypes

-- | A pure function for resolving variables.
getVariable :: Container ScopeInformation -- ^ The whole scope data.
            -> Text -- ^ Current scope name.
            -> Text -- ^ Full variable name.
            -> Either Doc PValue
getVariable scps scp fullvar = do
  (varscope, varname) <- case Text.splitOn "::" fullvar of
    [] -> Left "This doesn't make any sense in resolveVariable"
    [vn] -> pure (scp, vn) -- Non qualified variables
    rst -> pure (Text.intercalate "::" (filter (not . Text.null) (List.init rst)), List.last rst) -- qualified variables
  let extractVariable (varval :!: _ :!: _) = pure varval
  case scps ^? ix varscope . scopeVariables . ix varname of
    Just pp -> extractVariable pp
    Nothing -> -- check top level scope
      case scps ^? ix "::" . scopeVariables . ix varname of
        Just pp -> extractVariable pp
        Nothing -> Left ("Could not resolve variable" <+> pretty (UVariableReference fullvar) <+> "in context" <+> ppline varscope <+> "or root")

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
      (PString sa, PString sb)      -> CaseInsensitive.mk sa == CaseInsensitive.mk sb
      -- TODO, check if array / hash equality should be recursed
      -- for case insensitive matching
      _                             -> ra == rb

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
      pure (PBoolean (ra && rb))
    else pure (PBoolean False)
resolveExpression (Or a b) = do
  ra <- fmap pValue2Bool (resolveExpression a)
  if ra
    then pure (PBoolean True)
    else do
      rb <- fmap pValue2Bool (resolveExpression b)
      pure (PBoolean (ra || rb))
resolveExpression (LessThan a b) = numberCompare a b (<)
resolveExpression (MoreThan a b) = numberCompare a b (>)
resolveExpression (LessEqualThan a b) = numberCompare a b (<=)
resolveExpression (MoreEqualThan a b) = numberCompare a b (>=)
resolveExpression (RegexMatch a v@(Terminal (URegexp (CompRegex _ rv)))) = do
  ra <- fmap Text.encodeUtf8 (resolveExpressionString a)
  case Regex.execute' rv ra of
    Left (_,rr)    -> throwPosError ("Error when evaluating" <+> pretty v <+> ":" <+> ppstring rr)
    Right Nothing  -> pure $ PBoolean False
    Right (Just matches) -> do
      -- A bit of logic to save the capture variables.
      -- Note that this will pollute the namespace, as it should only
      -- happen in conditional expressions ...
      p <- use curPos
      ctype <- view cctype <$> getCurContainer
      let captures = zip (map (Text.pack . show) [(0 :: Int)..]) (map mkMatch (toList matches))
          mkMatch (offset, len) = PString (Text.decodeUtf8 (BS.take len (BS.drop offset ra))) :!: p :!: ctype
      scp <- getScopeName
      scopes . ix scp . scopeVariables %= HM.union (HM.fromList captures)
      pure $ PBoolean True
resolveExpression (RegexMatch _ t) = throwPosError ("The regexp matching operator expects a regular expression, not" <+> pretty t)
resolveExpression (NotRegexMatch a v) = resolveExpression (Not (RegexMatch a v))
resolveExpression (Equal a b) = do
  ra <- resolveExpression a
  rb <- resolveExpression b
  pure $ PBoolean $ puppetEquality ra rb
resolveExpression (Different a b) = resolveExpression (Not (Equal a b))
resolveExpression (Contains idx a) =
  resolveExpression a >>= \case
    PHash h -> do
      ridx <- resolveExpressionString idx
      case h ^. at ridx of
          Just _  -> pure (PBoolean True)
          Nothing -> pure (PBoolean False)
    PArray ar -> do
      ridx <- resolveExpression idx
      pure (PBoolean (ridx `V.elem` ar))
    PString st -> do
      ridx <- resolveExpressionString idx
      pure (PBoolean (ridx `Text.isInfixOf` st))
    src -> throwPosError ("Can't use the 'in' operator with" <+> pretty src)
resolveExpression (Lookup a idx) =
  resolveExpression a >>= \case
    PHash h -> do
      ridx <- resolveExpressionString idx
      case h ^. at ridx of
        Just v -> pure v
        Nothing -> do
          checkStrict
            ("Look up for an hash with the unknown key '" <> ppline ridx <> "' for" <+> pretty (PHash h))
            ("Can't find index '" <> ppline ridx <> "' in" <+> pretty (PHash h))
          pure PUndef
    PArray ar -> do
      ridx <- resolveExpression idx
      i <- case ridx ^? _Integer of
        Just n -> pure (fromIntegral n)
        _ -> throwPosError ("Need an integral number for indexing an array, not" <+> pretty ridx)
      let arl = V.length ar
      if arl <= i
        then throwPosError ("Out of bound indexing, array size is" <+> pretty arl <+> "index is" <+> pretty i)
        else pure (ar V.! i)
    src -> throwPosError ("This data can't be indexed:" <+> pretty src)
resolveExpression stmt@(ConditionalValue e conds) = do
  rese <- resolveExpression e
  let checkCond [] = throwPosError ("The selector didn't match anything for input" <+> pretty rese </> pretty stmt)
      checkCond ((SelectorDefault :!: ce) : _) = resolveExpression ce
      checkCond ((SelectorValue v@(URegexp (CompRegex _ rg)) :!: ce) : xs) = do
        rs <- fmap Text.encodeUtf8 (resolvePValueString rese)
        case Regex.execute' rg rs of
            Left (_,rr)    -> throwPosError ("Could not match" <+> pretty v <+> ":" <+> ppstring rr)
            Right Nothing  -> checkCond xs
            Right (Just _) -> resolveExpression ce
      checkCond ((SelectorType udt :!: ce) : xs) = do
        dt <- resolveDataType udt
        if datatypeMatch dt rese
          then resolveExpression ce
          else checkCond xs
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
    (PHash ha, PHash hb)   -> pure (PHash (ha <> hb))
    (PArray ha, PArray hb) -> pure (PArray (ha <> hb))
    _                      -> binaryOperation a b (+)
resolveExpression (Substraction a b)   = binaryOperation a b (-)
resolveExpression (Division a b)       = do
  ra <- resolveExpressionNumber a
  rb <- resolveExpressionNumber b
  case rb of
      0 -> throwPosError "Division by 0"
      _ -> case (,) `fmap` preview _Integer ra <*> preview _Integer rb of
             Just (ia, ib) -> pure $ PNumber $ fromIntegral (ia `div` ib)
             _             -> pure $ PNumber $ ra / rb
resolveExpression (Multiplication a b) = binaryOperation a b (*)
resolveExpression (Modulo a b)         = integerOperation a b mod
resolveExpression (RightShift a b)     = integerOperation a b (\x -> shiftR x . fromIntegral)
resolveExpression (LeftShift a b) = do
  ra <- resolveExpression a
  rb <- resolveExpression b
  case (ra, rb) of
      (PArray ha, v) -> pure (PArray (V.snoc ha v))
      _              -> integerOperation a b (\x -> shiftL x . fromIntegral)
resolveExpression a@(FunctionApplication e (Terminal (UHOLambdaCall hol))) = do
  unless (S.isNothing (hol ^. hoLambdaExpr))
    (throwPosError ("You can't combine chains of higher order functions (with .) and giving them parameters, in:" <+> pretty a))
  resolveValue (UHOLambdaCall (hol & hoLambdaExpr .~ S.Just e))
resolveExpression (FunctionApplication _ x) = throwPosError ("Expected function application here, not" <+> pretty x)
resolveExpression (Negate x) = PNumber . negate <$> resolveExpressionNumber x

-- | Resolves an 'UnresolvedValue' (terminal for the 'Expression' data type) into
-- a 'PValue'
resolveValue :: UnresolvedValue -> InterpreterMonad PValue
resolveValue (UNumber n) = pure (PNumber n)
resolveValue n@(URegexp _) = throwPosError ("Regular expressions are not allowed in this context: " <+> pretty n)
resolveValue (UBoolean x) = pure (PBoolean x)
resolveValue (UString x) = pure (PString x)
resolveValue UUndef = pure PUndef
resolveValue (UInterpolable vals) = fmap (PString . mconcat) (mapM resolveExpressionString (V.toList vals))
resolveValue (UResourceReference t e) = do
  r <- resolveExpressionStrings e
  case r of
    [s] -> pure (PResourceReference t (fixResourceName t s))
    _   -> pure (PArray (V.fromList (map (PResourceReference t . fixResourceName t) r)))
resolveValue (UArray a) = fmap PArray (V.mapM resolveExpression a)
resolveValue (UHash a) =
  fmap (PHash . HM.fromList) (mapM resPair (V.toList a))
  where
    resPair (k :!: v) = (,) `fmap` resolveExpressionString k <*> resolveExpression v
resolveValue (UVariableReference v) = resolveVariable v
resolveValue (UFunctionCall fname args) = resolveFunction fname args
resolveValue (UHOLambdaCall hol) = evaluateHFCPure hol
resolveValue (UDataType dt) = PType <$> resolveDataType dt

-- | Turns strings, numbers and booleans into 'Text', or throws an error.
resolvePValueString :: PValue -> InterpreterMonad Text
resolvePValueString (PString x) = pure x
resolvePValueString (PBoolean True) = pure "true"
resolvePValueString (PBoolean False) = pure "false"
resolvePValueString (PNumber x) = pure (scientific2text x)
resolvePValueString PUndef = do
  checkStrict
    "Resolving the keyword `undef` to the string \"undef\""
    "Strict mode won't convert the keyword `undef` to the string \"undef\""
  pure "undef"
resolvePValueString x = throwPosError ("Don't know how to convert this to a string:" <> line <>  pretty x)

-- | Turns everything it can into a number, or throws an error
resolvePValueNumber :: PValue -> InterpreterMonad Scientific
resolvePValueNumber x =
  case x ^? _Number of
    Just n  -> pure n
    Nothing -> throwPosError ("Don't know how to convert this to a number:" <> line <> pretty x)

-- | > resolveExpressionString = resolveExpression >=> resolvePValueString
resolveExpressionString :: Expression -> InterpreterMonad Text
resolveExpressionString = resolveExpression >=> resolvePValueString

-- | > resolveExpressionNumber = resolveExpression >=> resolvePValueNumber
resolveExpressionNumber :: Expression -> InterpreterMonad Scientific
resolveExpressionNumber = resolveExpression >=> resolvePValueNumber

-- | Just like 'resolveExpressionString', but accepts arrays.
resolveExpressionStrings :: Expression -> InterpreterMonad [Text]
resolveExpressionStrings x =
  resolveExpression x >>= \case
    PArray a -> mapM resolvePValueString (V.toList a)
    y -> fmap pure (resolvePValueString y)

-- | Turns a 'PValue' into a 'Bool' as explained in the reference documentation.
pValue2Bool :: PValue -> Bool
pValue2Bool PUndef       = False
pValue2Bool (PString "") = False
pValue2Bool (PBoolean x) = x
pValue2Bool _            = True

-- | This resolve function calls at the expression level.
resolveFunction :: Text -> V.Vector Expression -> InterpreterMonad PValue
resolveFunction "fqdn_rand" args = do
  let nbargs = V.length args
  when (nbargs < 1 || nbargs > 2) (throwPosError "fqdn_rand(): Expects one or two arguments")
  fqdn <- resolveVariable "::fqdn" >>= resolvePValueString
  (mx:targs) <- mapM resolveExpressionString (V.toList args)
  curmax <- case PString mx ^? _Integer of
    Just x -> pure x
    _ -> throwPosError ("fqdn_rand(): the first argument must be an integer, not" <+> ppline mx)
  let rargs = if null targs
                then [fqdn, ""]
                else fqdn : targs
      val = fromIntegral (fst (limitedRand (randInit myhash) (fromIntegral curmax)))
      myhash = toint (md5 (Text.encodeUtf8 fullstring)) :: Integer
      toint = BS.foldl' (\c nx -> c*256 + fromIntegral nx) 0
      fullstring = Text.intercalate ":" rargs
  pure (_Integer # val)
resolveFunction fname args =
  mapM resolveExpression (V.toList args) >>= resolveFunction' fname . map undefEmptyString
  where
    undefEmptyString PUndef = PString ""
    undefEmptyString x      = x

resolveFunction' :: Text -> [PValue] -> InterpreterMonad PValue
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
  if not (Text.null t) && Text.head t == '$' -- variable test
    then do
      scps <- use scopes
      scp <- getScopeName
      pure $ PBoolean $ case getVariable scps scp (Text.tail t) of
        Left _  -> False
        Right _ -> True
    else do -- resource test
      -- case 1, nested thingie
      nestedStuff <- use nestedDeclarations
      if has (ix (TopDefine, t)) nestedStuff || has (ix (TopClass, t)) nestedStuff
        then pure (PBoolean True)
        else do -- case 2, loaded class
          lc <- use loadedClasses
          if has (ix t) lc
            then pure (PBoolean True)
            else fmap PBoolean (isNativeType t)

resolveFunction' "defined" x = throwPosError ("defined(): expects a single resource reference, type or class name, and not" <+> pretty x)
resolveFunction' "fail" x = throwPosError ("fail:" <+> pretty x)
resolveFunction' "inline_template" [] = throwPosError "inline_template(): Expects at least one argument"
resolveFunction' "inline_template" templates = PString . mconcat <$> mapM (calcTemplate Left) templates
resolveFunction' "md5" [pstr] = fmap (PString . Text.decodeUtf8 . B16.encode . md5 . Text.encodeUtf8) (resolvePValueString pstr)
resolveFunction' "md5" _ = throwPosError "md5(): Expects a single argument"
resolveFunction' "regsubst" [ptarget, pregexp, preplacement] = resolveFunction' "regsubst" [ptarget, pregexp, preplacement, PString "G"]
resolveFunction' "regsubst" [ptarget, pregexp, preplacement, pflags] = do
  -- TODO handle all the flags
  -- http://docs.puppetlabs.com/references/latest/function.html#regsubst
  when (pflags /= "G") (use curPos >>= \p -> warn ("regsubst(): Currently only supports a single flag (G) " <> showPos (Tuple.fst p)))
  regexp      <- fmap Text.encodeUtf8 (resolvePValueString pregexp)
  replacement <- fmap Text.encodeUtf8 (resolvePValueString preplacement)
  let sub t = do
        t' <- fmap Text.encodeUtf8 (resolvePValueString t)
        case Regex.substituteCompile' regexp t' replacement of
          Left rr -> throwPosError ("regsubst():" <+> ppstring rr)
          Right x -> fmap PString (safeDecodeUtf8 x)
  case ptarget of
    PArray a -> fmap PArray (traverse sub a)
    s        -> sub s
resolveFunction' "regsubst" _ = throwPosError "regsubst(): Expects 3 or 4 arguments"
resolveFunction' "split" [psrc, psplt] = do
  src  <- fmap Text.encodeUtf8 (resolvePValueString psrc)
  splt <- fmap Text.encodeUtf8 (resolvePValueString psplt)
  case Regex.splitCompile' splt src of
    Left rr -> throwPosError ("splitCompile():" <+> ppstring rr)
    Right x -> fmap (PArray . V.fromList) (mapM (fmap PString . safeDecodeUtf8) x)
resolveFunction' "sha1" [pstr] = fmap (PString . Text.decodeUtf8 . B16.encode . sha1 . Text.encodeUtf8) (resolvePValueString pstr)
resolveFunction' "sha1" _ = throwPosError "sha1(): Expects a single argument"
resolveFunction' "shellquote" args = do
  sargs <- for args $ \arg ->
    case arg of
      PArray vals -> mapM resolvePValueString vals
      _           -> V.singleton <$> resolvePValueString arg
  let escape str | Text.all isSafe str            = str
                 | not (Text.any isDangerous str) = between "\"" str
                 | Text.any (== '\'') str          = between "\"" (Text.concatMap escapeDangerous str)
                 | otherwise                   = between "'" str
      isSafe x = Char.isAlphaNum x || x `elem` ("@%_+=:,./-" :: String)
      isDangerous x = x `elem` ("!\"`$\\" :: String)
      escapeDangerous x | isDangerous x = Text.snoc "\\" x
                        | otherwise = Text.singleton x
      between c s = c <> s <> c
  pure $ PString $ Text.unwords $ V.toList (escape <$> mconcat sargs)

resolveFunction' "mysql_password" [pstr] = fmap (PString . Text.decodeUtf8 . B16.encode . sha1 . sha1 . Text.encodeUtf8) (resolvePValueString pstr)
resolveFunction' "mysql_password" _ = throwPosError "mysql_password(): Expects a single argument"
resolveFunction' "file" args = do
  rebasefile <- fmap Text.pack <$> Operational.singleton RebaseFile
  let fixFilePath s | Text.null s = let rr = "Empty file path passed to the 'file' function" in checkStrict rr rr >> pure s
                    | Text.head s == '/' = pure (maybe s (<> s) rebasefile)
                    | otherwise = case Text.splitOn "/" s of
                                      (md:x:rst) -> do
                                          moduledir <- view modulesPath <$> getPuppetPaths
                                          pure (Text.intercalate "/" (Text.pack moduledir : md : "files" : x : rst))
                                      _ -> throwPosError ("file() argument invalid: " <> ppline s)
  mapM (resolvePValueString >=> fixFilePath) args >>= fmap PString . Operational.singleton . ReadFile

resolveFunction' "tagged" ptags = do
  tags <- fmap HS.fromList (mapM resolvePValueString ptags)
  scp <- getScopeName
  scpset <- use (scopes . ix scp . scopeExtraTags)
  pure (PBoolean (scpset `HS.intersection` tags == tags))
resolveFunction' "template" [] = throwPosError "template(): Expects at least one argument"
resolveFunction' "template" templates = PString . mconcat <$> mapM (calcTemplate Right) templates
resolveFunction' "versioncmp" [pa,pb] = do
  a <- resolvePValueString pa
  b <- resolvePValueString pb
  let parser x =
        case filter (null . snd) (readP_to_S parseVersion (Text.unpack x)) of
          ( (v, _) : _ ) -> v
          _              -> Version [] [] -- fallback :(
      va = parser a
      vb = parser b
  pure $ PString $ case compare va vb of
    EQ -> "0"
    LT -> "-1"
    GT -> "1"
resolveFunction' "versioncmp" _ = throwPosError "versioncmp(): Expects two arguments"
-- | Simplified implementation of sprintf
resolveFunction' "sprintf" (PString str:args) = sprintf str args
resolveFunction' "sprintf" _ = throwPosError "sprintf(): Expects a string as its first argument"
-- some custom functions
resolveFunction' "pdbresourcequery" [q]   = pdbresourcequery q Nothing
resolveFunction' "pdbresourcequery" [q,k] = fmap Just (resolvePValueString k) >>= pdbresourcequery q
resolveFunction' "pdbresourcequery" _     = throwPosError "pdbresourcequery(): Expects one or two arguments"
resolveFunction' "hiera"       [q]     = hieraCall QFirst   q Nothing Nothing Nothing
resolveFunction' "hiera"       [q,d]   = hieraCall QFirst   q (Just d) Nothing Nothing
resolveFunction' "hiera"       [q,d,o] = hieraCall QFirst   q (Just d) Nothing (Just o)
resolveFunction' "hiera_array" [q]     = hieraCall QUnique q Nothing  Nothing Nothing
resolveFunction' "hiera_array" [q,d]   = hieraCall QUnique q (Just d) Nothing Nothing
resolveFunction' "hiera_array" [q,d,o] = hieraCall QUnique q (Just d) Nothing (Just o)
resolveFunction' "hiera_hash"  [q]     = hieraCall QHash  q Nothing  Nothing Nothing
resolveFunction' "hiera_hash"  [q,d]   = hieraCall QHash  q (Just d) Nothing Nothing
resolveFunction' "hiera_hash"  [q,d,o] = hieraCall QHash  q (Just d) Nothing (Just o)
resolveFunction' "lookup"      [q]                        = hieraCall QFirst   q Nothing  Nothing Nothing
resolveFunction' "lookup"      [q, PType dt]              = hieraCall QFirst   q Nothing (Just dt) Nothing
resolveFunction' "lookup"      [q, PType dt, PString t,d] = hieraCall (fromMaybe QFirst (readQueryType t)) q (Just d) (Just dt) Nothing
resolveFunction' "lookup" _                               =  throwPosError "lookup(): Wrong set of arguments"

-- user functions
resolveFunction' fname args = Operational.singleton (ExternalFunction fname args)

pdbresourcequery :: PValue -> Maybe Text -> InterpreterMonad PValue
pdbresourcequery q mkey = do
  rrv <- case fromJSON (toJSON q) of
    Aeson.Success rq -> Operational.singleton (PDBGetResources rq)
    Aeson.Error rr   -> throwPosError ("Invalid resource query:" <+> ppstring rr)
  rv <- case fromJSON (toJSON rrv) of
    Aeson.Success x -> pure x
    Aeson.Error rr -> throwPosError ("For some reason we could not convert a resource list to Puppet internal values!!" <+> ppstring rr <+> pretty rrv)
  let extractSubHash :: Text -> PValue -> InterpreterMonad PValue
      extractSubHash ky (PHash h) =
        case h ^. at ky of
          Just val -> pure val
          Nothing -> throwPosError ("pdbresourcequery strange error, could not find key" <+> ppline ky <+> "in" <+> pretty (PHash h))
      extractSubHash _ x = throwPosError ("pdbresourcequery strange error, expected a hash, had" <+> pretty x)
  case mkey of
    Nothing  -> pure (PArray rv)
    (Just k) -> fmap PArray (V.mapM (extractSubHash k) rv)

calcTemplate :: (Text -> Either Text Text) -> PValue -> InterpreterMonad Text
calcTemplate templatetype templatename = do
  fname       <- resolvePValueString templatename
  stt         <- use identity
  Operational.singleton (ComputeTemplate (templatetype fname) stt)

resolveExpressionSE :: Expression -> InterpreterMonad PValue
resolveExpressionSE e =
  resolveExpression e >>= \case
    PArray _ -> throwPosError "The use of an array in a search expression is undefined"
    PHash _ -> throwPosError "The use of an array in a search expression is undefined"
    resolved -> pure resolved

-- | Turns an unresolved 'SearchExpression' from the parser into a fully
-- resolved 'RSearchExpression'.
resolveSearchExpression :: SearchExpression -> InterpreterMonad RSearchExpression
resolveSearchExpression AlwaysTrue = pure RAlwaysTrue
resolveSearchExpression (EqualitySearch a e) = REqualitySearch `fmap` pure a <*> resolveExpressionSE e
resolveSearchExpression (NonEqualitySearch a e) = RNonEqualitySearch `fmap` pure a <*> resolveExpressionSE e
resolveSearchExpression (AndSearch e1 e2) = RAndSearch `fmap` resolveSearchExpression e1 <*> resolveSearchExpression e2
resolveSearchExpression (OrSearch e1 e2) = ROrSearch `fmap` resolveSearchExpression e1 <*> resolveSearchExpression e2

-- | Turns a resource type and 'RSearchExpression' into something that can
-- be used in a PuppetDB query.
searchExpressionToPuppetDB :: Text -> RSearchExpression -> Query ResourceField
searchExpressionToPuppetDB rtype res =
  QAnd ( QEqual RType (capitalizeRT rtype) : mkSE res )
  where
    mkSE (RAndSearch a b)                = [QAnd (mkSE a ++ mkSE b)]
    mkSE (ROrSearch a b)                 = [QOr (mkSE a ++ mkSE b)]
    mkSE (RNonEqualitySearch a b)        = fmap QNot (mkSE (REqualitySearch a b))
    mkSE (REqualitySearch a (PString b)) = [QEqual (mkFld a) b]
    mkSE _                               = []
    mkFld "tag"   = RTag
    mkFld "title" = RTitle
    mkFld z       = RParameter z

-- | Checks whether a given 'Resource' matches a 'RSearchExpression'.
-- Note that the expression doesn't check for type, so you must filter the
-- resources by type beforehand, if needs be.
checkSearchExpression :: RSearchExpression -> Resource -> Bool
checkSearchExpression RAlwaysTrue _ = True
checkSearchExpression (RAndSearch a b) r = checkSearchExpression a r && checkSearchExpression b r
checkSearchExpression (ROrSearch a b) r = checkSearchExpression a r || checkSearchExpression b r
checkSearchExpression (REqualitySearch "tag" (PString s)) r = r ^. rtags . contains s
checkSearchExpression (REqualitySearch "tag" _) _ = False
checkSearchExpression (REqualitySearch "title" v) r =
  let nameequal = puppetEquality v (PString (r ^. rid . iname))
      aliasequal =
        case r ^. rattributes . at "alias" of
          Just a  -> puppetEquality v a
          Nothing -> False
  in nameequal || aliasequal
checkSearchExpression (REqualitySearch attributename v) r =
  case r ^. rattributes . at attributename of
    Nothing         -> False
    Just (PArray x) -> any (`puppetEquality` v) x
    Just x          -> puppetEquality x v
checkSearchExpression (RNonEqualitySearch attributename v) r
  | attributename  == "tag" = True
  | attributename  == "title" = not (checkSearchExpression (REqualitySearch attributename v) r)
  | otherwise =
      case r ^. rattributes . at attributename of
        Nothing         -> True
        Just (PArray x) -> not (all (`puppetEquality` v) x)
        Just x          -> not (puppetEquality x v)

resolveDataType :: UDataType -> InterpreterMonad DataType
resolveDataType ud
  = case ud of
      UDTType             -> pure DTType
      UDTString a b       -> pure (DTString a b)
      UDTInteger a b      -> pure (DTInteger a b)
      UDTFloat a b        -> pure (DTFloat a b)
      UDTBoolean          -> pure DTBoolean
      UDTArray dt a b     -> DTArray <$> resolveDataType dt <*> pure a <*> pure b
      UDTHash dt1 dt2 a b -> DTHash <$> resolveDataType dt1 <*> resolveDataType dt2 <*> pure a <*> pure b
      UDTUndef            -> pure DTUndef
      UDTScalar           -> pure DTScalar
      UDTData             -> pure DTData
      UDTOptional dt      -> DTOptional <$> resolveDataType dt
      UNotUndef           -> pure NotUndef
      UDTVariant vrs      -> DTVariant <$> traverse resolveDataType vrs
      UDTPattern a        -> pure (DTPattern a)
      -- will not crash as ens is nonempty
      UDTEnum ens         -> DTEnum . NE.fromList . sconcat <$> traverse resolveExpressionStrings ens
      UDTAny              -> pure DTAny
      UDTCollection       -> pure DTCollection

-- | Generates variable associations for evaluation of blocks.
-- Each item corresponds to an iteration in the calling block.
hfGenerateAssociations :: HOLambdaCall -> InterpreterMonad [[(Text, PValue)]]
hfGenerateAssociations hol = do
  sourceexpression <- case hol ^. hoLambdaExpr of
    S.Just x  -> pure x
    S.Nothing -> throwPosError ("No expression to run the function on" <+> pretty hol)
  sourcevalue <- resolveExpression sourceexpression
  let check Nothing _ = pure ()
      check (Just udtype) tocheck = do
        dtype <- resolveDataType udtype
        mapM_ (\v -> unless (datatypeMatch dtype v) (throwPosError (pretty v <+> "isn't of type" <+> pretty dtype))) tocheck
  case (sourcevalue, hol ^. hoLambdaParams) of
     (PArray pr, BPSingle (LParam mvtype varname)) -> do
       check mvtype pr
       pure (map (\x -> [(varname, x)]) (V.toList pr))
     (PArray pr, BPPair (LParam _ idx) (LParam mvtype var)) -> do
       check mvtype pr
       pure [ [(idx,PString (Text.pack (show i))),(var,v)]  |  (i,v) <- zip ([0..] :: [Int]) (V.toList pr) ]
     (PHash hh, BPSingle (LParam mvtype varname)) -> do
       check mvtype hh
       pure [ [(varname, PArray (V.fromList [PString k,v]))]  |  (k,v) <- HM.toList hh]
     (PHash hh, BPPair (LParam midxtype idx) (LParam mvtype var)) -> do
       check mvtype hh
       check midxtype (PString <$> HM.keys hh)
       pure [ [(idx,PString k),(var,v)]  |  (k,v) <- HM.toList hh]
     (invalid, _) -> throwPosError ("Can't iterate on this data type:" <+> pretty invalid)

-- | Sets the proper variables, and returns the scope variables the way
-- they were before being modified. This is a hack that ensures that
-- variables are local to the new scope.
--
-- It doesn't work at all like other Puppet parts, but consistency isn't
-- really expected here ...
hfSetvars :: [(Text, PValue)] -> InterpreterMonad (Container (Pair (Pair PValue PPosition) CurContainerDesc))
hfSetvars vals = do
  scp <- getScopeName
  p <- use curPos
  container <- getCurContainer
  save <- use (scopes . ix scp . scopeVariables)
  let hfSetvar (varname, varval) = scopes . ix scp . scopeVariables . at varname ?= (varval :!: p :!: (container ^. cctype))
  mapM_ hfSetvar vals
  pure save

-- | Restores what needs restoring. This will erase all allocations.
hfRestorevars :: Container (Pair (Pair PValue PPosition) CurContainerDesc) -> InterpreterMonad ()
hfRestorevars save = do
  scp <- getScopeName
  scopes . ix scp . scopeVariables .= save

-- | Evaluates a statement in "pure" mode. TODO
evalPureStatement :: Statement -> InterpreterMonad ()
evalPureStatement _ = throwPosError "So called 'pure' statements are not yet supported"

-- | This extracts the final expression from an HOLambdaCall.
-- When it does not exists, it checks if the last statement is in fact
-- a function call
transformPureHf :: HOLambdaCall -> InterpreterMonad (HOLambdaCall, Expression)
transformPureHf hol =
  case hol ^. hoLambdaLastExpr of
    S.Just x -> pure (hol, x)
    S.Nothing -> do
      let statements = hol ^. hoLambdaStatements
      if V.null statements
        then throwPosError ("The statement block must not be empty" <+> pretty hol)
        else case V.last statements of
          (MainFunctionDeclaration (MainFuncDecl fn args _)) ->
             let expr = Terminal (UFunctionCall fn args)
             in  pure (hol & hoLambdaStatements %~ V.init
                           & hoLambdaLastExpr .~ S.Just expr
                      , expr)
          _ -> throwPosError ("The statement block must end with an expression" <+> pretty hol)

-- | All the "higher order function" stuff, for "value" mode. In this case
-- we are in "pure" mode, and only a few statements are allowed.
evaluateHFCPure :: HOLambdaCall -> InterpreterMonad PValue
evaluateHFCPure hol' = do
  (hol, finalexpression) <- transformPureHf hol'
  varassocs <- hfGenerateAssociations hol
  let runblock :: [(Text, PValue)] -> InterpreterMonad PValue
      runblock assocs = do
          saved <- hfSetvars assocs
          V.mapM_ evalPureStatement (hol ^. hoLambdaStatements)
          r <- resolveExpression finalexpression
          hfRestorevars  saved
          pure r
  case hol ^. hoLambdaFunc of
    LambEach -> throwPosError "The 'each' function can't be used at the value level in language-puppet. Please use map."
    LambMap -> fmap (PArray . V.fromList) (mapM runblock varassocs)
    LambFilter -> do
      res <- mapM (fmap pValue2Bool . runblock) varassocs
      sourcevalue <- case hol ^. hoLambdaExpr of
        S.Just x  -> resolveExpression x
        S.Nothing -> throwPosError "Internal error evaluateHFCPure 1"
      case sourcevalue of
        PArray ar -> pure $ PArray $ V.map fst $ V.filter snd $ V.zip ar (V.fromList res)
        PHash  hh -> pure $ PHash  $ HM.fromList $ map fst $ filter snd $ zip (HM.toList hh) res
        x         -> throwPosError ("Can't iterate on this data type:" <+> pretty x)
    x -> throwPosError ("This type of function is not supported yet by language-puppet!" <+> pretty x)

-- | Checks that a value matches a puppet datatype
datatypeMatch :: DataType -> PValue -> Bool
datatypeMatch dt v =
  case dt of
    DTType               -> has _PType v
    DTUndef              -> v == PUndef
    NotUndef             -> v /= PUndef
    DTString mmin mmax   -> boundedBy _PString Text.length mmin mmax
    DTInteger mmin mmax  -> boundedBy (_PNumber . to Scientific.toBoundedInteger . _Just) identity mmin mmax
    DTFloat mmin mmax    -> boundedBy _PNumber Scientific.toRealFloat mmin mmax
    DTBoolean            -> has _PBoolean v
    DTArray sdt mi mmx   -> container (_PArray . to V.toList) (datatypeMatch sdt) mi mmx
    DTHash kt sdt mi mmx -> container (_PHash . to itoList) (\(k,a) -> datatypeMatch kt (PString k) && datatypeMatch sdt a) mi mmx
    DTScalar             -> datatypeMatch (DTVariant (DTInteger Nothing Nothing :| [DTString Nothing Nothing, DTBoolean])) v
    DTData               -> datatypeMatch (DTVariant (DTScalar :| [DTArray DTData 0 Nothing, DTHash DTScalar DTData 0 Nothing])) v
    DTOptional sdt       -> datatypeMatch (DTVariant (DTUndef :| [sdt])) v
    DTVariant sdts       -> any (`datatypeMatch` v) sdts
    DTEnum lst           -> maybe False (`elem` lst) (v ^? _PString)
    DTAny                -> True
    DTCollection         -> datatypeMatch (DTVariant (DTArray DTData 0 Nothing :| [DTHash DTScalar DTData 0 Nothing])) v
    DTPattern patterns   -> maybe False (\str -> any (checkPattern (Text.encodeUtf8 str)) patterns) (v ^? _PString)
  where
    checkPattern str (CompRegex _ ptrn) =
      case Regex.execute' ptrn str of
        Right (Just _) -> True
        _              -> False
    container :: Fold PValue [a] -> (a -> Bool) -> Int -> Maybe Int -> Bool
    container f c mi mmx =
      let lst = v ^. f
          ln = length lst
      in  ln >= mi && (fmap (ln <=) mmx /= Just False) && all c lst
    boundedBy :: Ord b => Fold PValue a -> (a -> b) -> Maybe b -> Maybe b -> Bool
    boundedBy prm f mmin mmax =
      fromMaybe False $ do
        vr <- f <$> v ^? prm
        pure $ and (catMaybes [fmap (vr >=) mmin, fmap (vr <=) mmax])
