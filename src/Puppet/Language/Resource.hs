{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Puppet.Language.Resource
  ( Resource (..),
    HasResource (..),
    RIdentifier (..),
    HasRIdentifier (..),
    LinkInformation (..),
    HasLinkInformation (..),
    LinkType (..),
    Virtuality (..),
    CurContainerDesc (..),
    FinalCatalog,
    EdgeMap,
  )
where

import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified GHC.Exts as Exts
import Puppet.Language.Core
import Puppet.Language.Value
import qualified Text.Megaparsec.Pos as Pos
import XPrelude

rel2text :: LinkType -> Text
rel2text RNotify = "notify"
rel2text RRequire = "require"
rel2text RBefore = "before"
rel2text RSubscribe = "subscribe"

data Virtuality
  = -- | Normal resource, that will be included in the catalog.
    Normal
  | -- | Type for virtual resources.
    Virtual
  | -- | Type for exported resources.
    Exported
  | -- | These are resources that are exported AND realized in the catalog.
    ExportedRealized
  deriving (Eq, Show)

data CurContainerDesc
  = -- | Contained at node or root level.
    ContRoot
  | -- | Contained in a class.
    ContClass !Text
  | -- | Contained in a define, along with the position where this define was ... defined
    ContDefine
      !Text
      !Text
      !PPosition
  | -- | Dummy container for imported resources, so that we know we must update the nodename
    ContImported !CurContainerDesc
  | -- | This one is used when finalizing imported resources, and contains the current node name
    ContImport
      !NodeName
      !CurContainerDesc
  deriving (Eq, Generic, Ord, Show)

instance Pretty CurContainerDesc where
  pretty (ContImport p x) = magenta "import" <> braces (ppline p) <> braces (pretty x)
  pretty (ContImported x) = magenta "imported" <> braces (pretty x)
  pretty ContRoot = dullyellow "::"
  pretty (ContClass cname) = dullyellow "class" <+> dullgreen (ppline cname)
  pretty (ContDefine dtype dname _) = pretty (PResourceReference dtype dname)

-- | Relationship/ordering between resources.
data LinkType
  = -- | Applies a resource after the target resource.
    RRequire
  | -- | Applies a resource before the target resource.
    RBefore
  | -- | Applies a resource before the target resource. The target resource refreshes if the notifying resource changes.
    RNotify
  | -- | Applies a resource after the target resource. The subscribing resource refreshes if the target resource changes.
    RSubscribe
  deriving (Show, Eq, Generic)

instance Hashable LinkType

instance FromJSON LinkType where
  parseJSON (String "require") = return RRequire
  parseJSON (String "notify") = return RNotify
  parseJSON (String "subscribe") = return RSubscribe
  parseJSON (String "before") = return RBefore
  parseJSON _ = panic "invalid linktype"

instance ToJSON LinkType where
  toJSON = String . rel2text

instance Pretty LinkType where
  pretty RNotify = "~>"
  pretty RRequire = "<-"
  pretty RBefore = "->"
  pretty RSubscribe = "<~"

-- | In Puppet, a resource is identified by a name and a type.
data RIdentifier = RIdentifier
  { _itype :: !Text,
    _iname :: !Text
  }
  deriving (Show, Eq, Generic, Ord)

instance Pretty RIdentifier where
  pretty (RIdentifier t n) = pretty (PResourceReference t n)

instance FromJSON RIdentifier where
  parseJSON (Object v) = RIdentifier <$> v .: "type" <*> v .: "title"
  parseJSON _ = fail "invalid resource"

instance ToJSON RIdentifier where
  toJSON (RIdentifier t n) = object [("type", String t), ("title", String n)]

instance Hashable RIdentifier

-- | A fully resolved puppet resource that will be used in the 'FinalCatalog'.
data Resource = Resource
  { -- | Resource name.
    _rid :: !RIdentifier,
    -- | All the resource aliases
    _ralias :: !(HashSet Text),
    -- | Resource parameters.
    _rattributes :: !(Container PValue),
    -- | Resource relations.
    _rrelations :: !(HashMap RIdentifier (HashSet LinkType)),
    -- | Resource scope when it was defined, the real container will be the first item
    _rscope :: ![CurContainerDesc],
    -- | Virtuality.
    _rvirtuality :: !Virtuality,
    -- | Tags.
    _rtags :: !(HashSet Text),
    -- | Source code position of the resource definition.
    _rpos :: !PPosition,
    -- | The node were this resource was created, if remote
    _rnode :: !NodeName
  }
  deriving (Eq, Show)

makeClassy ''Resource
makeClassy ''RIdentifier

resourceRelations :: Resource -> [(RIdentifier, LinkType)]
resourceRelations = concatMap expandSet . Map.toList . view rrelations
  where
    expandSet (ri, lts) = [(ri, lt) | lt <- Set.toList lts]

meta :: Resource -> Doc
meta r = showPPos (r ^. rpos) <+> red node <+> green (brackets scp)
  where
    node = ppline (r ^. rnode)
    scp = "Scope" <+> pretty (r ^.. rscope . folded . filtered (/= ContRoot) . to pretty)

resourceBody :: Resource -> Doc
resourceBody r =
  virtuality <> blue (ppline (r ^. rid . iname)) <> ":" <+> meta r <> line <> containerComma'' insde <> ";"
  where
    virtuality = case r ^. rvirtuality of
      Normal -> mempty
      Virtual -> dullred "@"
      Exported -> dullred "@@"
      ExportedRealized -> dullred "<@@>"
    insde = alignlst dullblue attriblist1 ++ alignlst dullmagenta attriblist2
    alignlst col = map (first (fill maxalign . col . ppline))
    attriblist1 = Exts.sortWith fst $ Map.toList (r ^. rattributes) ++ aliasdiff
    aliasWithoutTitle = r ^. ralias & contains (r ^. rid . iname) .~ False
    aliasPValue = aliasWithoutTitle & PArray . Vector.fromList . map PString . Set.toList
    aliasdiff
      | Set.null aliasWithoutTitle = []
      | otherwise = [("alias", aliasPValue)]
    attriblist2 = map totext (resourceRelations r)
    totext (RIdentifier t n, lt) = (rel2text lt, PResourceReference t n)
    maxalign = max (maxalign' attriblist1) (maxalign' attriblist2)
    maxalign' [] = 0
    maxalign' x = maximum . map (Text.length . fst) $ x

instance Pretty Resource where
  prettyList lst =
    let grouped = Map.toList $ Map.fromListWith (++) [(r ^. rid . itype, [r]) | r <- lst] :: [(Text, [Resource])]
        sorted = Exts.sortWith fst (map (second (Exts.sortWith (view (rid . iname)))) grouped)
        showGroup :: (Text, [Resource]) -> Doc
        showGroup (rt, res) = dullyellow (ppline rt) <+> lbrace <> line <> indent 2 (vcat (map resourceBody res)) <> line <> rbrace
     in vcat (map showGroup sorted)
  pretty r = dullyellow (ppline (r ^. rid . itype)) <+> lbrace <> line <> indent 2 (resourceBody r) <> line <> rbrace

instance ToJSON Resource where
  toJSON r =
    object
      [ ("type", String $ r ^. rid . itype),
        ("title", String $ r ^. rid . iname),
        ("aliases", toJSON $ r ^. ralias),
        ("exported", Bool $ r ^. rvirtuality == Exported),
        ("tags", toJSON $ r ^. rtags),
        ("parameters", Object $ KM.fromHashMapText (fmap toJSON (r ^. rattributes) `Map.union` relations)),
        ("sourceline", r ^. rpos . _1 . _sourceLine . to (toJSON . Pos.unPos)),
        ("sourcefile", r ^. rpos . _1 . _sourceName . to toJSON)
      ]
    where
      relations = r ^. rrelations & Map.fromListWith (Vector.++) . concatMap changeRelations . Map.toList & fmap toValue
      toValue v
        | Vector.length v == 1 = Vector.head v
        | otherwise = Array v
      changeRelations :: (RIdentifier, HashSet LinkType) -> [(Text, Vector Value)]
      changeRelations (k, v) = do
        c <- Set.toList v
        return (rel2text c, Vector.singleton (String (rid2text k)))
      rid2text :: RIdentifier -> Text
      rid2text (RIdentifier t n) = capitalizeRT t `Text.append` "[" `Text.append` capn `Text.append` "]"
        where
          capn =
            if t == "classe"
              then capitalizeRT n
              else n

instance FromJSON Resource where
  parseJSON (Object v) = do
    isExported <- v .: "exported"
    let virtuality =
          if isExported
            then Exported
            else Normal
        getResourceIdentifier :: PValue -> Maybe RIdentifier
        getResourceIdentifier (PString x) =
          let (restype, brckts) = Text.breakOn "[" x
              rna
                | Text.null brckts = Nothing
                | Text.null restype = Nothing
                | Text.last brckts == ']' = Just (Text.tail (Text.init brckts))
                | otherwise = Nothing
           in case rna of
                Just resname -> Just (RIdentifier (Text.toLower restype) (Text.toLower resname))
                _ -> Nothing
        getResourceIdentifier _ = Nothing
        -- TODO : properly handle metaparameters
        separate :: (Container PValue, HashMap RIdentifier (HashSet LinkType)) -> Text -> PValue -> (Container PValue, HashMap RIdentifier (HashSet LinkType))
        separate (curAttribs, curRelations) k val = case (fromJSON (String k), getResourceIdentifier val) of
          (Success rel, Just ri) -> (curAttribs, curRelations & at ri . non mempty . contains rel .~ True)
          _ -> (curAttribs & at k ?~ val, curRelations)
    (attribs, relations) <- Map.foldlWithKey' separate (mempty, mempty) <$> v .: "parameters"
    contimport <- v .:? "certname" .!= "unknown"
    Resource
      <$> (RIdentifier <$> fmap Text.toLower (v .: "type") <*> v .: "title")
      <*> v .:? "aliases" .!= mempty
      <*> pure attribs
      <*> pure relations
      <*> pure [ContImport contimport ContRoot]
      <*> pure virtuality
      <*> v .: "tags"
      <*> (toPPos <$> v .:? "sourcefile" .!= "null" <*> v .:? "sourceline" .!= 1)
      <*> pure contimport
  parseJSON _ = mempty

type FinalCatalog = HashMap RIdentifier Resource

-- | Relationship/ordering information between two resources (used in the 'EdgeMap').
data LinkInformation = LinkInformation
  { _linksrc :: !RIdentifier,
    _linkdst :: !RIdentifier,
    _linkType :: !LinkType,
    _linkPos :: !PPosition
  }
  deriving (Show)

makeClassy ''LinkInformation

type EdgeMap = HashMap RIdentifier [LinkInformation]
