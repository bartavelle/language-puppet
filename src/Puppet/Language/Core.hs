module Puppet.Language.Core

where

import           XPrelude

import           Data.Aeson
import qualified Data.Char           as Char
import qualified Data.HashMap.Strict as Map
import qualified Data.Text           as Text
import qualified Data.Tuple.Strict   as Tuple
import qualified GHC.Show            as Show (Show (..))
import           Text.Megaparsec.Pos


showPos :: Position -> Doc
showPos = blue . pptext . sourcePosPretty

-- | showing a position interval only show the first position
showPPos :: PPosition -> Doc
showPPos = showPos . Tuple.fst

-- | Generates an initial position interval based on a filename.
initialPPos :: FilePath -> PPosition
initialPPos x =
    let i = initialPos x
    in (i :!: i)

-- | A pair containing the start and end of a given token.
type PPosition = Pair Position Position

-- | Position in a puppet file. Currently an alias to 'SourcePos'.
type Position = SourcePos

type NodeName = Text
type Scope = Text

data CompRegex = CompRegex !Text !Regex

instance Show CompRegex where
  show (CompRegex t _) = show t

instance Eq CompRegex where
    (CompRegex a _) == (CompRegex b _) = a == b

instance FromJSON CompRegex where
  parseJSON = panic "Can't deserialize a regular expression"

instance ToJSON CompRegex where
  toJSON (CompRegex t _) = toJSON t

instance Pretty CompRegex where
  pretty (CompRegex r _) = pretty '/' <> ppline r <> pretty '/'

-- | Extremely hacky escaping system for text values.
stringEscape :: Text -> Text
stringEscape = Text.concatMap escapeChar
  where
    escapeChar '"' = "\\\""
    escapeChar '\n' = "\\n"
    escapeChar '\t' = "\\t"
    escapeChar '\r' = "\\r"
    escapeChar x = Text.singleton x
{-# INLINE stringEscape #-}

-- | Capitalize resource type and convert into a 'Doc'.
capitalizeR :: Text -> Doc
capitalizeR = dullyellow . ppline . capitalizeRT

-- | Properly capitalizes resource types.
capitalizeRT :: Text -> Text
capitalizeRT = Text.intercalate "::" . map capitalize' . Text.splitOn "::"
  where
    capitalize' :: Text -> Text
    capitalize' t | Text.null t = Text.empty
                  | otherwise = Text.cons (Char.toUpper (Text.head t)) (Text.tail t)

containerComma'' :: Pretty a => [(Doc, a)] -> Doc
containerComma'' x = indent 4 ins
  where
    ins = mconcat $ intersperse (comma <> line <> mempty) (fmap showC x)
    showC (a,b) = a <+> "=>" <+> pretty b

containerComma' :: Pretty a => [(Doc, a)] -> Doc
containerComma' = braces . containerComma''

containerComma :: Pretty a => Container a -> Doc
containerComma hm = containerComma' (fmap (\(a,b) -> (fill maxalign (ppline a), b)) hml)
  where
      hml = Map.toList hm
      maxalign = maximum (fmap (Text.length . fst) hml)


-- Lens
_sourceName :: Lens' Position String
_sourceName = lens sourceName (\s n -> s { sourceName = n })

_sourceLine :: Lens' Position Pos
_sourceLine = lens sourceLine (\s l -> s { sourceLine = l })

_sourceColumn :: Lens' Position Pos
_sourceColumn = lens sourceColumn (\s c -> s { sourceColumn = c })

-- | Generates a 'PPosition' based on a filename and line number.
toPPos :: Text -> Int -> PPosition
toPPos fl ln =
  let p = (initialPos (toS fl)) { sourceLine = mkPos $ fromIntegral (max 1 ln) }
  in  (p :!: p)
