{-# OPTIONS_GHC -fno-warn-orphans #-}
module Puppet.Parser.PrettyPrinter where

import Puppet.PP
import Data.Monoid
import Puppet.Parser.Types
import qualified Data.Vector as V
import qualified Data.Text as T
import Data.Tuple.Strict (Pair ( (:!:) ))
import qualified Data.Tuple.Strict as S
import qualified Data.Maybe.Strict as S

capitalize :: T.Text -> Doc
capitalize = dullyellow . text . T.unpack . capitalizeRT

parensList :: Pretty a => V.Vector a -> Doc
parensList = tupled . map pretty . V.toList

hashComma :: (Pretty a, Pretty b) => V.Vector (Pair a b) -> Doc
hashComma = encloseSep lbrace rbrace comma . map showC . V.toList
    where
        showC (a :!: b) = pretty a <+> text "=>" <+> pretty b

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
    pretty (PValue a)             = pretty a

instance Pretty SearchExpression where
    pretty (EqualitySearch t e) = text (T.unpack t) <+> text "==" <+> pretty e
    pretty (NonEqualitySearch t e) = text (T.unpack t) <+> text "!=" <+> pretty e
    pretty AlwaysTrue = empty
    pretty (AndSearch s1 s2) = parens (pretty s1) <+> text "and" <+> parens (pretty s2)
    pretty (OrSearch s1 s2) = parens (pretty s1) <+> text "and" <+> parens (pretty s2)

instance Pretty UValue where
    pretty (UBoolean True)  = dullmagenta $ text "true"
    pretty (UBoolean False) = dullmagenta $ text "false"
    pretty (UString s) = dullcyan (text (show s))
    pretty (UInterpolable v) = char '"' <> hcat (map specific (V.toList v)) <> char '"'
        where
            specific (UString s) = dullcyan (text (tail (init (show s))))
            specific (UVariableReference vr) = dullblue (text "${" <> text (T.unpack vr) <> char '}')
            specific x = bold (red (pretty x))
    pretty UUndef = dullmagenta (text "undef")
    pretty (UResourceReference t n) = capitalize t <> brackets (pretty n)
    pretty (UArray v) = list (map pretty (V.toList v))
    pretty (UHash g) = hashComma g
    pretty (URegexp r _) = char '/' <> text (T.unpack r) <> char '/'
    pretty (UVariableReference v) = dullblue (char '$' <> text (T.unpack v))
    pretty (UFunctionCall f args) = showFunc f args

instance Pretty SelectorCase where
    pretty SelectorDefault = dullmagenta (text "default")
    pretty (SelectorValue v) = pretty v

showPos :: Position -> Doc
showPos p = green (char '#' <+> string (show p))

showPPos :: PPosition -> Doc
showPPos p = green (char '#' <+> string (show (S.fst p)))

showAss :: V.Vector (Pair T.Text Expression) -> Doc
showAss v = folddoc (\a b -> a <> char ',' <$> b) rh lst
    where
        folddoc _ _ [] = empty
        folddoc docAppend docGen (x:xs) = foldl docAppend (docGen x) (map docGen xs)
        lst = V.toList v
        maxlen = maximum (map (T.length . S.fst) lst)
        rh (k :!: val) = dullblue (fill maxlen (text (T.unpack k))) <+> text "=>" <+> pretty val

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
    pretty (NodeMatch m r) = pretty (URegexp m r)

instance Pretty Statement where
    pretty (ConditionalStatement conds p)
        | V.null conds = empty
        | otherwise = text "if" <+> pretty firstcond <+> showPPos p <+> braceStatements firststts <$> vcat (map rendernexts xs)
        where
            ( (firstcond :!: firststts) : xs ) = V.toList conds
            rendernexts (PValue (UBoolean True) :!: st) = text "else" <+> braceStatements st
            rendernexts (c :!: st) | V.null st = empty
                                   | otherwise = text "elsif" <+> pretty c <+> braceStatements st
    pretty (MainFunctionCall funcname args p) = showFunc funcname args <+> showPPos p
    pretty (DefaultDeclaration rtype defaults p) = capitalize rtype <+> nest 2 (char '{' <+> showPPos p <$> showAss defaults) <$> char '}'
    pretty (ResourceOverride rtype rnames overs p) = pretty (UResourceReference rtype rnames) <+> nest 2 (char '{' <+> showPPos p <$> showAss overs) <$> char '}'
    pretty (ResourceDeclaration rtype rname args virt p) = nest 2 (red vrt <> dullgreen (text (T.unpack rtype)) <+> char '{' <+> showPPos p
                                                                <$> nest 2 (pretty rname <> char ':' <$> showAss args))
                                                                <$> char '}'
        where
            vrt = case virt of
                      Normal -> empty
                      Virtual -> char '@'
                      Exported -> text "@@"
                      ExportedRealized -> text "!!"
    pretty (DefineDeclaration cname args stts p) = dullyellow (text "define") <+> dullgreen (ttext cname) <> showArgs args <+> showPPos p <$> braceStatements stts
    pretty (ClassDeclaration cname args inherit stts p) = dullyellow (text "class") <+> dullgreen (text (T.unpack cname)) <> showArgs args <> inheritance <+> showPPos p
                                                               <$> braceStatements stts
        where
            inheritance = case inherit of
                              S.Nothing -> empty
                              S.Just x -> empty <+> text "inherits" <+> text (T.unpack x)
    pretty (VariableAssignment a b p) = dullblue (char '$' <> text (T.unpack a)) <+> char '=' <+> pretty b <+> showPPos p
    pretty (Node nodename stmts i p) = dullyellow (text "node") <+> pretty nodename <> inheritance <+> showPPos p <$> braceStatements stmts
        where
            inheritance = case i of
                              S.Nothing -> empty
                              S.Just n -> empty <+> text "inherits" <+> pretty n
    pretty (Dependency (st :!: sn) (dt :!: dn) p) = pretty (UResourceReference st sn) <+> text "->" <+> pretty (UResourceReference dt dn) <+> showPPos p
    pretty (TopContainer a b) = text "TopContainer:" <+> braces ( nest 2 (string "TOP" <$> braceStatements a <$> string "STATEMENT" <$> pretty b))
    pretty (ResourceCollection coltype restype search overrides p) = capitalize restype <> enc (pretty search) <+> overs
        where
            overs | V.null overrides = showPPos p
                  | otherwise = nest 2 (char '{' <+> showPPos p <$> showAss overrides) <$> char '}'
            enc = case coltype of
                      Collector         -> enclose (text "<|")   (text "|>")
                      ExportedCollector -> enclose (text "<<|")  (text "|>>")

ppStatements :: V.Vector Statement -> Doc
ppStatements = vcat . map pretty . V.toList

