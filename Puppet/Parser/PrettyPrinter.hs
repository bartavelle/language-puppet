{-# OPTIONS_GHC -fno-warn-orphans #-}
module Puppet.Parser.PrettyPrinter where

import           Puppet.Prelude               hiding (empty, (<$>))

import qualified Data.Maybe.Strict            as S
import qualified Data.Text                    as Text
import qualified Data.Tuple.Strict            as Tuple
import qualified Data.Vector                  as V
import           Text.PrettyPrint.ANSI.Leijen ((<$>))

import           Puppet.Parser.Types
import           Puppet.PP

capitalize :: Text -> Doc
capitalize = dullyellow . text . Text.unpack . capitalizeRT

parensList :: Pretty a => V.Vector a -> Doc
parensList = tupled . fmap pretty . V.toList

hashComma :: (Pretty a, Pretty b) => V.Vector (Pair a b) -> Doc
hashComma = encloseSep lbrace rbrace comma . fmap showC . V.toList
    where
        showC (a :!: b) = pretty a <+> text "=>" <+> pretty b

-- Extremely hacky escaping system
stringEscape :: Text -> Text
stringEscape = Text.concatMap escapeChar
    where
        escapeChar '"'  = "\\\""
        escapeChar '\n' = "\\n"
        escapeChar '\t' = "\\t"
        escapeChar '\r' = "\\r"
        escapeChar x    = Text.singleton x
{-# INLINE stringEscape #-}

instance Pretty DataType where
  pretty t = case t of
               DTType              -> "Type"
               DTString ma mb      -> bounded "String" ma mb
               DTInteger ma mb     -> bounded "Integer" ma mb
               DTFloat ma mb       -> bounded "Float" ma mb
               DTBoolean           -> "Boolean"
               DTArray dt mi mmx   -> "Array" <> list (pretty dt : pretty mi : maybe [] (pure . pretty) mmx)
               DTHash kt dt mi mmx -> "Hash" <> list (pretty kt : pretty dt : pretty mi : maybe [] (pure . pretty) mmx)
               DTUndef             -> "Undef"
               DTScalar            -> "Scalar"
               DTData              -> "Data"
               DTOptional o        -> "Optional" <> brackets (pretty o)
               NotUndef            -> "NotUndef"
               DTVariant vs        -> "Variant" <> list (foldMap (pure . pretty) vs)
               DTPattern vs        -> "Pattern" <> list (foldMap (pure . pretty) vs)
               DTEnum tx           -> "Enum" <> list (foldMap (pure . text . Text.unpack) tx)
               DTAny               -> "Any"
               DTCollection        -> "Collection"
    where
      bounded :: (Pretty a, Pretty b) => Doc -> Maybe a -> Maybe b -> Doc
      bounded s ma mb = s <> case (ma, mb) of
                               (Just a, Nothing) -> list [pretty a]
                               (Just a, Just b)  -> list [pretty a, pretty b]
                               _                 -> mempty

instance Pretty Expression where
    pretty (Equal a b)            = parens (pretty a <+> text "==" <+> pretty b)
    pretty (Different a b)        = parens (pretty a <+> text "!=" <+> pretty b)
    pretty (And a b)              = parens (pretty a <+> text "and" <+> pretty b)
    pretty (Or a b)               = parens (pretty a <+> text "or" <+> pretty b)
    pretty (LessThan a b)         = parens (pretty a <+> text "<" <+> pretty b)
    pretty (MoreThan a b)         = parens (pretty a <+> text ">" <+> pretty b)
    pretty (LessEqualThan a b)    = parens (pretty a <+> text "<=" <+> pretty b)
    pretty (MoreEqualThan a b)    = parens (pretty a <+> text ">=" <+> pretty b)
    pretty (RegexMatch a b)       = parens (pretty a <+> text "=~" <+> pretty b)
    pretty (NotRegexMatch a b)    = parens (pretty a <+> text "!~" <+> pretty b)
    pretty (Contains a b)         = parens (pretty a <+> text "in" <+> pretty b)
    pretty (Addition a b)         = parens (pretty a <+> text "+" <+> pretty b)
    pretty (Substraction a b)     = parens (pretty a <+> text "-" <+> pretty b)
    pretty (Division a b)         = parens (pretty a <+> text "/" <+> pretty b)
    pretty (Multiplication a b)   = parens (pretty a <+> text "*" <+> pretty b)
    pretty (Modulo a b)           = parens (pretty a <+> text "%" <+> pretty b)
    pretty (RightShift a b)       = parens (pretty a <+> text ">>" <+> pretty b)
    pretty (LeftShift a b)        = parens (pretty a <+> text "<<" <+> pretty b)
    pretty (Lookup a b)           = pretty a <> brackets (pretty b)
    pretty (ConditionalValue a b) = parens (pretty a <+> text "?" <+> hashComma b)
    pretty (Negate a)             = text "-" <+> parens (pretty a)
    pretty (Not a)                = text "!" <+> parens (pretty a)
    pretty (Terminal a)           = pretty a
    pretty (FunctionApplication e1 e2) = parens (pretty e1) <> text "." <> pretty e2

instance Pretty LambdaFunc where
    pretty LambEach   = bold $ red $ text "each"
    pretty LambMap    = bold $ red $ text "map"
    pretty LambReduce = bold $ red $ text "reduce"
    pretty LambFilter = bold $ red $ text "filter"
    pretty LambSlice  = bold $ red $ text "slice"
    pretty LambLookup = bold $ red $ text "lookup"

instance Pretty LambdaParameters where
    pretty b = magenta (char '|') <+> vars <+> magenta (char '|')
        where
            pmspace = foldMap ((<> " ") . pretty)
            vars = case b of
                       BPSingle (LParam mt v) -> pmspace mt <> pretty (UVariableReference v)
                       BPPair (LParam mt1 v1) (LParam mt2 v2) -> pmspace mt1 <> pretty (UVariableReference v1) <> comma <+> pmspace mt2 <> pretty (UVariableReference v2)

instance Pretty SearchExpression where
    pretty (EqualitySearch t e) = text (Text.unpack t) <+> text "==" <+> pretty e
    pretty (NonEqualitySearch t e) = text (Text.unpack t) <+> text "!=" <+> pretty e
    pretty AlwaysTrue = empty
    pretty (AndSearch s1 s2) = parens (pretty s1) <+> text "and" <+> parens (pretty s2)
    pretty (OrSearch s1 s2) = parens (pretty s1) <+> text "and" <+> parens (pretty s2)

instance Pretty UnresolvedValue where
    pretty (UBoolean True)  = dullmagenta $ text "true"
    pretty (UBoolean False) = dullmagenta $ text "false"
    pretty (UString s) = char '"' <> dullcyan (ttext (stringEscape s)) <> char '"'
    pretty (UNumber n) = cyan (ttext (scientific2text n))
    pretty (UInterpolable v) = char '"' <> hcat (map specific (V.toList v)) <> char '"'
        where
            specific (Terminal (UString s)) = dullcyan (ttext (stringEscape s))
            specific (Terminal (UVariableReference vr)) = dullblue (text "${" <> text (Text.unpack vr) <> char '}')
            specific (Lookup (Terminal (UVariableReference vr)) (Terminal x)) = dullblue (text "${" <> text (Text.unpack vr) <> char '[' <> pretty x <> "]}")
            specific x = bold (red (pretty x))
    pretty UUndef = dullmagenta (text "undef")
    pretty (UResourceReference t n) = capitalize t <> brackets (pretty n)
    pretty (UArray v) = list (map pretty (V.toList v))
    pretty (UHash g) = hashComma g
    pretty (URegexp r) = pretty r
    pretty (UVariableReference v) = dullblue (char '$' <> text (Text.unpack v))
    pretty (UFunctionCall f args) = showFunc f args
    pretty (UHOLambdaCall c) = pretty c

instance Pretty CompRegex where
    pretty (CompRegex r _) = char '/' <> text (Text.unpack r) <> char '/'

instance Pretty HOLambdaCall where
    pretty (HOLambdaCall hf me bp stts mee) = pretty hf <> mme <+> pretty bp <+> nest 2 (char '{' <$> ppStatements stts <> mmee) <$> char '}'
        where
            mme = case me of
                      S.Just x  -> mempty <+> pretty x
                      S.Nothing -> mempty
            mmee = case mee of
                       S.Just x  -> mempty </> pretty x
                       S.Nothing -> mempty
instance Pretty SelectorCase where
    pretty SelectorDefault   = dullmagenta (text "default")
    pretty (SelectorType t)  = pretty t
    pretty (SelectorValue v) = pretty v

instance Pretty LinkType where
    pretty RNotify    = "~>"
    pretty RRequire   = "<-"
    pretty RBefore    = "->"
    pretty RSubscribe = "<~"

instance Pretty ArrowOp where
    pretty AssignArrow = "=>"
    pretty AppendArrow = "+>"

showPos :: Position -> Doc
showPos p = green (char '#' <+> string (show p))

showPPos :: PPosition -> Doc
showPPos p = green (char '#' <+> string (show (Tuple.fst p)))

showAss :: V.Vector AttributeDecl -> Doc
showAss vx = folddoc (\a b -> a <> char ',' <$> b) prettyDecl (V.toList vx)
    where
        folddoc _ _ []            = empty
        folddoc acc docGen (x:xs) = foldl acc (docGen x) (map docGen xs)
        maxlen = maximum (fmap (\(AttributeDecl k _ _) -> Text.length k) vx)
        prettyDecl (AttributeDecl k op v) = dullblue (fill maxlen (ttext k)) <+> pretty op <+> pretty v

showArgs :: V.Vector (Pair (Pair Text (S.Maybe DataType)) (S.Maybe Expression)) -> Doc
showArgs vec = tupled (map ra lst)
    where
        lst = V.toList vec
        maxlen = maximum (map (Text.length . Tuple.fst . Tuple.fst) lst)
        ra (argname :!: mtype :!: rval)
          = dullblue (char '$' <> foldMap (\t -> pretty t <+> empty) mtype
                               <> fill maxlen (text (Text.unpack argname)))
                               <> foldMap (\v -> empty <+> char '=' <+> pretty v) rval

showFunc :: Text -> V.Vector Expression -> Doc
showFunc funcname args = bold (red (text (Text.unpack funcname))) <> parensList args
braceStatements :: V.Vector Statement -> Doc
braceStatements stts = nest 2 (char '{' <$> ppStatements stts) <$> char '}'

instance Pretty NodeDesc where
    pretty NodeDefault   = dullmagenta (text "default")
    pretty (NodeName n)  = pretty (UString n)
    pretty (NodeMatch r) = pretty (URegexp r)

instance Pretty Statement where
    pretty (HigherOrderLambdaDeclaration (HigherOrderLambdaDecl c p)) = pretty c <+> showPPos p
    pretty (ConditionalDeclaration (ConditionalDecl conds p))
        | V.null conds = empty
        | otherwise = text "if" <+> pretty firstcond <+> showPPos p <+> braceStatements firststts <$> vcat (map rendernexts xs)
        where
            ( (firstcond :!: firststts) : xs ) = V.toList conds
            rendernexts (Terminal (UBoolean True) :!: st) = text "else" <+> braceStatements st
            rendernexts (c :!: st) | V.null st = empty
                                   | otherwise = text "elsif" <+> pretty c <+> braceStatements st
    pretty (MainFunctionDeclaration (MainFuncDecl funcname args p)) = showFunc funcname args <+> showPPos p
    pretty (ResourceDefaultDeclaration (ResDefaultDecl rtype defaults p)) = capitalize rtype <+> nest 2 (char '{' <+> showPPos p <$> showAss defaults) <$> char '}'
    pretty (ResourceOverrideDeclaration (ResOverrideDecl rtype rnames overs p)) = pretty (UResourceReference rtype rnames) <+> nest 2 (char '{' <+> showPPos p <$> showAss overs) <$> char '}'
    pretty (ResourceDeclaration (ResDecl rtype rname args virt p)) = nest 2 (red vrt <> dullgreen (text (Text.unpack rtype)) <+> char '{' <+> showPPos p
                                                                           <$> nest 2 (pretty rname <> char ':' <$> showAss args))
                                                                           <$> char '}'
        where
            vrt = case virt of
                      Normal           -> empty
                      Virtual          -> char '@'
                      Exported         -> text "@@"
                      ExportedRealized -> text "!!"
    pretty (DefineDeclaration (DefineDecl cname args stts p)) = dullyellow (text "define") <+> dullgreen (ttext cname) <> showArgs args <+> showPPos p <$> braceStatements stts
    pretty (ClassDeclaration (ClassDecl cname args inherit stts p)) = dullyellow (text "class") <+> dullgreen (text (Text.unpack cname)) <> showArgs args <> inheritance <+> showPPos p
                                                               <$> braceStatements stts
        where
            inheritance = case inherit of
                              S.Nothing -> empty
                              S.Just x -> empty <+> text "inherits" <+> text (Text.unpack x)
    pretty (VarAssignmentDeclaration (VarAssignDecl a b p)) = dullblue (char '$' <> text (Text.unpack a)) <+> char '=' <+> pretty b <+> showPPos p
    pretty (NodeDeclaration (NodeDecl nodename stmts i p)) = dullyellow (text "node") <+> pretty nodename <> inheritance <+> showPPos p <$> braceStatements stmts
        where
            inheritance = case i of
                              S.Nothing -> empty
                              S.Just n -> empty <+> text "inherits" <+> pretty n
    pretty (DependencyDeclaration (DepDecl (st :!: sn) (dt :!: dn) lt p)) = pretty (UResourceReference st sn) <+> pretty lt <+> pretty (UResourceReference dt dn) <+> showPPos p
    pretty (TopContainer a b) = text "TopContainer:" <+> braces ( nest 2 (string "TOP" <$> braceStatements a <$> string "STATEMENT" <$> pretty b))
    pretty (ResourceCollectionDeclaration (ResCollDecl coltype restype search overrides p)) = capitalize restype <> enc (pretty search) <+> overs
        where
            overs | V.null overrides = showPPos p
                  | otherwise = nest 2 (char '{' <+> showPPos p <$> showAss overrides) <$> char '}'
            enc = case coltype of
                      Collector         -> enclose (text "<|")   (text "|>")
                      ExportedCollector -> enclose (text "<<|")  (text "|>>")

ppStatements :: V.Vector Statement -> Doc
ppStatements = vcat . map pretty . V.toList
