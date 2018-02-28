{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE GADTs             #-}
module Puppet.Interpreter.PrettyPrinter () where

import           XPrelude

import qualified Data.Aeson                 as Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL

import           Puppet.Interpreter.Types
import           PuppetDB

instance Pretty TemplateSource where
  pretty (Inline s) = pretty (PString s)
  pretty (Filename s) = pptext s

instance Pretty TopLevelType where
    pretty TopNode   = dullyellow "node"
    pretty TopDefine = dullyellow "define"
    pretty TopClass  = dullyellow "class"

instance Pretty ResDefaults where
    pretty (ResDefaults t _ v p) = capitalizeR t <+> showPPos p <> line <> containerComma v

instance Pretty ResourceModifier where
    pretty (ResourceModifier rt ModifierMustMatch RealizeVirtual (REqualitySearch "title" (PString x)) _ p) = "realize" <> parens (pretty (PResourceReference rt x)) <+> showPPos p
    pretty _ = "TODO pretty ResourceModifier"

instance Pretty RSearchExpression where
  pretty (REqualitySearch a v) = ppline a <+> "==" <+> pretty v
  pretty (RNonEqualitySearch a v) = ppline a <+> "!=" <+> pretty v
  pretty (RAndSearch a b) = parens (pretty a) <+> "&&" <+> parens (pretty b)
  pretty (ROrSearch a b) = parens (pretty a) <+> "||" <+> parens (pretty b)
  pretty RAlwaysTrue = mempty

pf :: Doc -> [Doc] -> Doc
pf fn args = bold (red fn) <> tupled (map pretty args)

showQuery :: ToJSON a => Query a -> Doc
showQuery = ppstring . BSL.unpack . Aeson.encode

instance Pretty (InterpreterInstr a) where
  pretty PuppetPaths = pf "PuppetPathes" []
  pretty RebaseFile = pf "RebaseFile" []
  pretty IsStrict = pf "IsStrict" []
  pretty GetNativeTypes = pf "GetNativeTypes" []
  pretty (GetStatement tlt nm) = pf "GetStatement" [pretty tlt,ppline nm]
  pretty (ComputeTemplate src _) = pf "ComputeTemplate" [pretty src]
  pretty (ExternalFunction fn args)  = pf (ppline fn) (map pretty args)
  pretty GetNodeName                 = pf "GetNodeName" []
  pretty (HieraQuery _ q _)          = pf "HieraQuery" [ppline q]
  pretty GetCurrentCallStack         = pf "GetCurrentCallStack" []
  pretty (ErrorThrow rr)             = pf "ErrorThrow" [getError rr]
  pretty (ErrorCatch _ _)            = pf "ErrorCatch" []
  pretty (WriterTell t)              = pf "WriterTell" (map (pretty . view _2) t)
  pretty (WriterPass _)              = pf "WriterPass" []
  pretty (WriterListen _)            = pf "WriterListen" []
  pretty PDBInformation              = pf "PDBInformation" []
  pretty (PDBReplaceCatalog _)       = pf "PDBReplaceCatalog" ["..."]
  pretty (PDBReplaceFacts _)         = pf "PDBReplaceFacts" ["..."]
  pretty (PDBDeactivateNode n)       = pf "PDBDeactivateNode" [ppline n]
  pretty (PDBGetFacts q)             = pf "PDBGetFacts" [showQuery q]
  pretty (PDBGetResources q)         = pf "PDBGetResources" [showQuery q]
  pretty (PDBGetNodes q)             = pf "PDBGetNodes" [showQuery q]
  pretty PDBCommitDB                 = pf "PDBCommitDB" []
  pretty (PDBGetResourcesOfNode n q) = pf "PDBGetResourcesOfNode" [ppline n, showQuery q]
  pretty (ReadFile f)                = pf "ReadFile" (map ppline f)
  pretty (TraceEvent e)              = pf "TraceEvent" [ppstring e]
  pretty (IsIgnoredModule m)         = pf "IsIgnoredModule" [ppline m]
  pretty (IsExternalModule m)        = pf "IsExternalModule" [ppline m]

instance Pretty LinkInformation where
    pretty (LinkInformation lsrc ldst ltype lpos) = pretty lsrc <+> pretty ltype <+> pretty ldst <+> showPPos lpos
