{-# OPTIONS_GHC -fno-warn-orphans #-}
module Puppet.Parser.PrettyPrinter where

import qualified Data.Maybe.Strict            as S
import qualified Data.Text                    as T
import           Data.Tuple.Strict            (Pair ((:!:)))
import qualified Data.Tuple.Strict            as S
import qualified Data.Vector                  as V
import           Puppet.Parser.Types
import           Puppet.PP
import           Puppet.Utils
import           Text.PrettyPrint.ANSI.Leijen ((<$>))
import           Prelude                      hiding ((<$>))

capitalize :: T.Text -> Doc
capitalize = dullyellow . text . T.unpack . capitalizeRT

parensList :: Pretty a => V.Vector a -> Doc
parensList = tupled . map pretty . V.toList

hashComma :: (Pretty a, Pretty b) => V.Vector (Pair a b) -> Doc
hashComma = encloseSep lbrace rbrace comma . map showC . V.toList
    where
        showC (a :!: b) = pretty a <+> text "=>" <+> pretty b

-- Extremely hacky escaping system
stringEscape :: T.Text -> T.Text
stringEscape = T.concatMap escapeChar
    where
        escapeChar '"' = "\\\""
        escapeChar '\n' = "\\n"
        escapeChar '\t' = "\\t"
        escapeChar '\r' = "\\r"
        escapeChar x = T.singleton x
{-# INLINE stringEscape #-}

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

instance Pretty HigherFuncType where
    pretty HFEach   = bold $ red $ text "each"
    pretty HFMap    = bold $ red $ text "map"
    pretty HFReduce = bold $ red $ text "reduce"
    pretty HFFilter = bold $ red $ text "filter"
    pretty HFSlice  = bold $ red $ text "slice"

instance Pretty BlockParameters where
    pretty b = magenta (char '|') <+> vars <+> magenta (char '|')
        where
            vars = case b of
                       BPSingle v -> pretty (UVariableReference v)
                       BPPair v1 v2 -> pretty (UVariableReference v1) <> comma <+> pretty (UVariableReference v2)

instance Pretty SearchExpression where
    pretty (EqualitySearch t e) = text (T.unpack t) <+> text "==" <+> pretty e
    pretty (NonEqualitySearch t e) = text (T.unpack t) <+> text "!=" <+> pretty e
    pretty AlwaysTrue = empty
    pretty (AndSearch s1 s2) = parens (pretty s1) <+> text "and" <+> parens (pretty s2)
    pretty (OrSearch s1 s2) = parens (pretty s1) <+> text "and" <+> parens (pretty s2)

instance Pretty UValue where
    pretty (UBoolean True)  = dullmagenta $ text "true"
    pretty (UBoolean False) = dullmagenta $ text "false"
    pretty (UString s) = char '"' <> dullcyan (ttext (stringEscape s)) <> char '"'
    pretty (UNumber n) = cyan (ttext (scientific2text n))
    pretty (UInterpolable v) = char '"' <> hcat (map specific (V.toList v)) <> char '"'
        where
            specific (Terminal (UString s)) = dullcyan (ttext (stringEscape s))
            specific (Terminal (UVariableReference vr)) = dullblue (text "${" <> text (T.unpack vr) <> char '}')
            specific (Lookup (Terminal (UVariableReference vr)) (Terminal x)) = dullblue (text "${" <> text (T.unpack vr) <> char '[' <> pretty x <> "]}")
            specific x = bold (red (pretty x))
    pretty UUndef = dullmagenta (text "undef")
    pretty (UResourceReference t n) = capitalize t <> brackets (pretty n)
    pretty (UArray v) = list (map pretty (V.toList v))
    pretty (UHash g) = hashComma g
    pretty (URegexp (CompRegex r _)) = char '/' <> text (T.unpack r) <> char '/'
    pretty (UVariableReference v) = dullblue (char '$' <> text (T.unpack v))
    pretty (UFunctionCall f args) = showFunc f args
    pretty (UHFunctionCall c) = pretty c

instance Pretty HFunctionCall where
    pretty (HFunctionCall hf me bp stts mee) = pretty hf <> mme <+> pretty bp <+> nest 2 (char '{' <$> ppStatements stts <> mmee) <$> char '}'
        where
            mme = case me of
                      S.Just x -> mempty <+> pretty x
                      S.Nothing -> mempty
            mmee = case mee of
                       S.Just x -> mempty </> pretty x
                       S.Nothing -> mempty
instance Pretty SelectorCase where
    pretty SelectorDefault = dullmagenta (text "default")
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
showPPos p = green (char '#' <+> string (show (S.fst p)))

showAss :: V.Vector AttributeDecl -> Doc
showAss vx = folddoc (\a b -> a <> char ',' <$> b) prettyDecl (V.toList vx)
    where
        folddoc _ _ [] = empty
        folddoc acc docGen (x:xs) = foldl acc (docGen x) (map docGen xs)
        maxlen = maximum (fmap (\(AttributeDecl k _ _) -> T.length k) vx)
        prettyDecl (AttributeDecl k op v) = dullblue (fill maxlen (ttext k)) <+> pretty op <+> pretty v

showArgs :: V.Vector (Pair T.Text (S.Maybe Expression)) -> Doc
showArgs vec = tupled (map ra lst)
    where
        lst = V.toList vec
        maxlen = maximum (map (T.length . S.fst) lst)
        ra (argname :!: rval) = dullblue (char '$' <> fill maxlen (text (T.unpack argname)))
                                    <> case rval of
                                           S.Nothing -> empty
                                           S.Just v  -> empty <+> char '=' <+> pretty v

showFunc :: T.Text -> V.Vector Expression -> Doc
showFunc funcname args = bold (red (text (T.unpack funcname))) <> parensList args
braceStatements :: V.Vector Statement -> Doc
braceStatements stts = nest 2 (char '{' <$> ppStatements stts) <$> char '}'

instance Pretty NodeDesc where
    pretty NodeDefault     = dullmagenta (text "default")
    pretty (NodeName n)    = pretty (UString n)
    pretty (NodeMatch r)   = pretty (URegexp r)

instance Pretty Statement where
    pretty (SHFunctionCall (SFC c p)) = pretty c <+> showPPos p
    pretty (ConditionalStatement (CondStatement conds p))
        | V.null conds = empty
        | otherwise = text "if" <+> pretty firstcond <+> showPPos p <+> braceStatements firststts <$> vcat (map rendernexts xs)
        where
            ( (firstcond :!: firststts) : xs ) = V.toList conds
            rendernexts (Terminal (UBoolean True) :!: st) = text "else" <+> braceStatements st
            rendernexts (c :!: st) | V.null st = empty
                                   | otherwise = text "elsif" <+> pretty c <+> braceStatements st
    pretty (MainFunctionCall (MFC funcname args p)) = showFunc funcname args <+> showPPos p
    pretty (DefaultDeclaration (DefaultDec rtype defaults p)) = capitalize rtype <+> nest 2 (char '{' <+> showPPos p <$> showAss defaults) <$> char '}'
    pretty (ResourceOverride (ResOver rtype rnames overs p)) = pretty (UResourceReference rtype rnames) <+> nest 2 (char '{' <+> showPPos p <$> showAss overs) <$> char '}'
    pretty (ResourceDeclaration (ResDec rtype rname args virt p)) = nest 2 (red vrt <> dullgreen (text (T.unpack rtype)) <+> char '{' <+> showPPos p
                                                                           <$> nest 2 (pretty rname <> char ':' <$> showAss args))
                                                                           <$> char '}'
        where
            vrt = case virt of
                      Normal -> empty
                      Virtual -> char '@'
                      Exported -> text "@@"
                      ExportedRealized -> text "!!"
    pretty (DefineDeclaration (DefineDec cname args stts p)) = dullyellow (text "define") <+> dullgreen (ttext cname) <> showArgs args <+> showPPos p <$> braceStatements stts
    pretty (ClassDeclaration (ClassDecl cname args inherit stts p)) = dullyellow (text "class") <+> dullgreen (text (T.unpack cname)) <> showArgs args <> inheritance <+> showPPos p
                                                               <$> braceStatements stts
        where
            inheritance = case inherit of
                              S.Nothing -> empty
                              S.Just x -> empty <+> text "inherits" <+> text (T.unpack x)
    pretty (VariableAssignment (VarAss a b p)) = dullblue (char '$' <> text (T.unpack a)) <+> char '=' <+> pretty b <+> showPPos p
    pretty (Node (Nd nodename stmts i p)) = dullyellow (text "node") <+> pretty nodename <> inheritance <+> showPPos p <$> braceStatements stmts
        where
            inheritance = case i of
                              S.Nothing -> empty
                              S.Just n -> empty <+> text "inherits" <+> pretty n
    pretty (Dependency (Dep (st :!: sn) (dt :!: dn) lt p)) = pretty (UResourceReference st sn) <+> pretty lt <+> pretty (UResourceReference dt dn) <+> showPPos p
    pretty (TopContainer a b) = text "TopContainer:" <+> braces ( nest 2 (string "TOP" <$> braceStatements a <$> string "STATEMENT" <$> pretty b))
    pretty (ResourceCollection (RColl coltype restype search overrides p)) = capitalize restype <> enc (pretty search) <+> overs
        where
            overs | V.null overrides = showPPos p
                  | otherwise = nest 2 (char '{' <+> showPPos p <$> showAss overrides) <$> char '}'
            enc = case coltype of
                      Collector         -> enclose (text "<|")   (text "|>")
                      ExportedCollector -> enclose (text "<<|")  (text "|>>")

ppStatements :: V.Vector Statement -> Doc
ppStatements = vcat . map pretty . V.toList
