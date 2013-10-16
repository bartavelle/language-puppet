{-# OPTIONS_GHC -fno-warn-orphans #-}
module Puppet.Interpreter.PrettyPrinter(containerComma) where

import Puppet.PP
import Puppet.Parser.Types
import Puppet.Interpreter.Types
import Puppet.Parser.PrettyPrinter

import Data.Monoid
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Control.Arrow (first,second)
import Control.Lens
import Data.List
import GHC.Exts

containerComma'' :: Pretty a => [(Doc, a)] -> Doc
containerComma'' x = indent 2 ins
    where
        ins = mconcat $ intersperse (comma <$> empty) (map showC x)
        showC (a,b) = a <+> text "=>" <+> pretty b

containerComma' :: Pretty a => [(Doc, a)] -> Doc
containerComma' = braces . containerComma''

containerComma :: Pretty a => Container a -> Doc
containerComma hm = containerComma' (map (\(a,b) -> (fill maxalign (ttext a), b)) hml)
    where
        hml = HM.toList hm
        maxalign = maximum (map (T.length . fst) hml)

instance Pretty PValue where
    pretty (PBoolean True)  = dullmagenta $ text "true"
    pretty (PBoolean False) = dullmagenta $ text "false"
    pretty (PString s) = dullcyan (text (show s))
    pretty PUndef = dullmagenta (text "undef")
    pretty (PResourceReference t n) = capitalize t <> brackets (text (T.unpack n))
    pretty (PArray v) = list (map pretty (V.toList v))
    pretty (PHash g) = containerComma g

instance Pretty TopLevelType where
    pretty TopNode     = dullyellow (text "node")
    pretty TopDefine   = dullyellow (text "define")
    pretty TopClass    = dullyellow (text "class")
    pretty TopSpurious = dullyellow (text "spurious")

instance Pretty RIdentifier where
    pretty (RIdentifier t n) = pretty (PResourceReference t n)

meta :: Resource -> Doc
meta r = showPPos (r ^. rpos) <+> (green (node <> brackets cont <+> brackets scp) )
    where
        node = maybe mempty ((<+> mempty) . red . ttext) (r ^. rnode)
        cont = case r ^. rcontainer of
                   ContRoot -> magenta "top level"
                   ContClass cname -> magenta "class" <+> ttext cname
                   ContDefine t n -> pretty (PResourceReference t n)
                   ContImported -> magenta "imported"
        scp = "Scope" <+> pretty (r ^.. rscope . folded . filtered (/="::") . to (white . ttext))

resourceBody :: Resource -> Doc
resourceBody r = virtuality <> blue (ttext (r ^. rid . iname)) <> ":" <+> meta r <$> containerComma'' insde <> ";"
        where
           virtuality = case r ^. rvirtuality of
                            Normal -> empty
                            Virtual -> dullred "@"
                            Exported -> dullred "@@"
                            ExportedRealized -> dullred "<@@>"
           insde = alignlst dullblue attriblist1 ++ alignlst dullmagenta attriblist2
           alignlst col = map (first (fill maxalign . col . ttext))
           attriblist1 = sortWith fst $ HM.toList (r ^. rattributes) ++ aliasdiff
           aliasWithoutTitle = r ^. ralias & contains (r ^. rid . iname) .~ False
           aliasPValue = aliasWithoutTitle & PArray . V.fromList . map PString . HS.toList
           aliasdiff | HS.null aliasWithoutTitle = [("alias", aliasPValue)]
                     | otherwise = []
           attriblist2 = map totext (resourceRelations r)
           totext (RIdentifier t n, lt) = (rel2text lt , PResourceReference t n)
           maxalign = max (maxalign' attriblist1) (maxalign' attriblist2)
           maxalign' [] = 0
           maxalign' x = maximum . map (T.length . fst) $ x

instance Pretty Resource where
    prettyList lst =
       let grouped = HM.toList $ HM.fromListWith (++) [ (r ^. rid . itype, [r]) | r <- lst ] :: [ (T.Text, [Resource]) ]
           sorted = sortWith fst (map (second (sortWith (_iname . _rid) )) grouped)
           showGroup :: (T.Text, [Resource]) -> Doc
           showGroup (rt, res) = dullyellow (ttext rt) <+> lbrace <$> indent 2 (vcat (map resourceBody res)) <$> rbrace
       in  vcat (map showGroup sorted)
    pretty r = dullyellow (ttext (r ^. rid . itype)) <+> lbrace <$> indent 2 (resourceBody r) <$> rbrace

instance Pretty CurContainerDesc where
    pretty ContImported = magenta "imported"
    pretty ContRoot = dullyellow (text "::")
    pretty (ContClass cname) = dullyellow (text "class") <+> dullgreen (text (T.unpack cname))
    pretty (ContDefine dtype dname) = pretty (PResourceReference dtype dname)

instance Pretty ResDefaults where
    pretty (ResDefaults t _ v p) = capitalize t <+> showPPos p <$> containerComma v

instance Pretty ResourceModifier where
    pretty (ResourceModifier rt ModifierMustMatch RealizeVirtual (REqualitySearch "title" (PString x)) _ p) = "realize" <> parens (pretty (PResourceReference rt x)) <+> showPPos p
    pretty _ = "TODO pretty ResourceModifier"

instance Pretty RSearchExpression where
    pretty (REqualitySearch a v) = ttext a <+> "==" <+> pretty v
    pretty (RNonEqualitySearch a v) = ttext a <+> "!=" <+> pretty v
    pretty (RAndSearch a b) = parens (pretty a) <+> "&&" <+> parens (pretty b)
    pretty (ROrSearch a b) = parens (pretty a) <+> "||" <+> parens (pretty b)
    pretty RAlwaysTrue = mempty
