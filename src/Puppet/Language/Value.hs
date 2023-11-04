{-# LANGUAGE TemplateHaskell   #-}
module Puppet.Language.Value

where

import           XPrelude

import           Data.Aeson
import qualified Data.Aeson.KeyMap                as KM
import           Data.Aeson.TH
import           Data.Scientific (isInteger)
import           Foreign.Ruby.Helpers

import           Puppet.Language.Core

data DataType
    = DTType
    | DTString (Maybe Int) (Maybe Int)
    | DTInteger (Maybe Int) (Maybe Int)
    | DTFloat (Maybe Double) (Maybe Double)
    | DTBoolean
    | DTArray DataType Int (Maybe Int)
    | DTHash DataType DataType Int (Maybe Int)
    | DTUndef
    | DTScalar
    | DTData
    | DTOptional DataType
    | NotUndef
    | DTVariant (NonEmpty DataType)
    | DTPattern (NonEmpty CompRegex)
    | DTEnum (NonEmpty Text)
    | DTAny
    | DTCollection
    | DTRegexp (Maybe CompRegex)
    | DTDeferred
    | DTSensitive DataType
    deriving (Show, Eq)

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
    DTEnum tx           -> "Enum" <> list (foldMap (pure . ppline) tx)
    DTAny               -> "Any"
    DTCollection        -> "Collection"
    DTRegexp mr         -> "Regex" <> foldMap (brackets . pretty) mr
    DTDeferred          -> "Deferred"
    DTSensitive o       -> "Sensitive" <> brackets (pretty o)
    where
      bounded :: (Pretty a, Pretty b) => Doc -> Maybe a -> Maybe b -> Doc
      bounded s ma mb = s <> case (ma, mb) of
        (Just a, Nothing) -> list [pretty a]
        (Just a, Just b)  -> list [pretty a, pretty b]
        _                 -> mempty

$(deriveJSON defaultOptions ''DataType)

-- | A puppet value.
data PValue
  = PBoolean !Bool
  | PUndef
  | PString !Text
  | PResourceReference !Text !Text
  | PArray !(Vector PValue)
  | PHash !(Container PValue)
  | PNumber !Scientific
  | PType !DataType
  | PRegexp !CompRegex
  | PSensitive !PValue
  deriving (Eq, Show)

makePrisms ''PValue

instance Pretty PValue where
  pretty (PBoolean True) = dullmagenta $ "true"
  pretty (PBoolean False) = dullmagenta $ "false"
  pretty (PString s) = dullcyan (ppline (stringEscape s))
  pretty (PNumber n) = cyan (ppline (scientific2text n))
  pretty PUndef = dullmagenta "undef"
  pretty (PResourceReference t n) = capitalizeR t <> brackets (ppline n)
  pretty (PArray v) = list (map pretty (toList v))
  pretty (PHash g) = containerComma g
  pretty (PType dt) = pretty dt
  pretty (PRegexp cr) = pretty cr
  pretty (PSensitive _) = red "[SENSITIVE]"

instance IsString PValue where
  fromString = PString . toS

instance Pretty (HashMap Text PValue) where
  pretty = containerComma

_PValueNumber :: Prism' PValue Scientific
_PValueNumber = prism num2PValue toNumber
  where
    num2PValue :: Scientific -> PValue
    num2PValue = PNumber
    toNumber :: PValue -> Either PValue Scientific
    toNumber (PNumber n) = Right n
    toNumber p@(PString x) = case text2Scientific x of
      Just o -> Right o
      _ -> Left p
    toNumber p = Left p

_ScientificInteger :: Prism' Scientific Integer
_ScientificInteger = prism fromIntegral $ \n ->
    if isInteger n
      then Right (truncate n)
      else Left n

_PValueInteger :: Prism' PValue Integer
_PValueInteger = _PValueNumber . _ScientificInteger

instance FromJSON PValue where
  parseJSON Null = return PUndef
  parseJSON (Number n) = return $ PNumber n
  parseJSON (String s) = return (PString s)
  parseJSON (Bool b) = return (PBoolean b)
  parseJSON (Array v) = fmap PArray (mapM parseJSON v)
  parseJSON (Object o) | KM.size o == 1 && KM.keys o == ["regexp"] = o .: "regexp"
  parseJSON (Object o) = fmap (PHash . KM.toHashMapText) (traverse parseJSON o)

instance ToJSON PValue where
  toJSON (PType t) = toJSON t
  toJSON (PBoolean b) = Bool b
  toJSON PUndef = Null
  toJSON (PString s) = String s
  toJSON (PResourceReference _ _) = Null -- TODO
  toJSON (PArray r) = Array (fmap toJSON r)
  toJSON (PHash x) = Object (KM.fromHashMapText (fmap toJSON x))
  toJSON (PNumber n) = Number n
  toJSON (PRegexp r) = object [("regexp", toJSON r)]
  toJSON (PSensitive x) = toJSON x

instance ToRuby PValue where
    toRuby = toRuby . toJSON
instance FromRuby PValue where
  fromRuby = fmap chk . fromRuby
    where
      chk (Left x) = Left x
      chk (Right x) = case fromJSON x of
        Error rr    -> Left rr
        Success suc -> Right suc
