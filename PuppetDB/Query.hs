module PuppetDB.Query where

import Data.Aeson
import qualified Data.Text as T
import qualified Data.Vector as V
import Control.Applicative

data ResourcesQuery3 = ResOr    [ResourcesQuery3]
                     | ResAnd   [ResourcesQuery3]
                     | ResNot   ResourcesQuery3
                     | ResEqual ResField3 T.Text
                     | ResMatch ResField3 T.Text

data ResField3 = RFTag
               | RFCertname
               | RFParameter T.Text
               | RFType
               | RFTitle
               | RFExported
               | RFFile
               | RFLine

instance ToJSON ResField3 where
    toJSON RFTag           = "tag"
    toJSON RFCertname      = "certname"
    toJSON (RFParameter t) = toJSON ["parameter", t]
    toJSON RFType          = "type"
    toJSON RFTitle         = "title"
    toJSON RFExported      = "exported"
    toJSON RFFile          = "file"
    toJSON RFLine          = "line"

instance FromJSON ResField3 where
    parseJSON "tag"      = return RFTag
    parseJSON "certname" = return RFCertname
    parseJSON "type"     = return RFType
    parseJSON "title"    = return RFTitle
    parseJSON "exported" = return RFExported
    parseJSON "file"     = return RFFile
    parseJSON "line"     = return RFLine
    parseJSON (Array xs) = case V.toList xs of
                               ["parameter", x] -> RFParameter <$> parseJSON x
                               _ -> fail "Invalid field syntax"
    parseJSON _ = fail "invalid field"

instance ToJSON ResourcesQuery3 where
    toJSON (ResOr qs)          = toJSON ("or" : map toJSON qs)
    toJSON (ResAnd qs)         = toJSON ("and" : map toJSON qs)
    toJSON (ResNot q)          = toJSON [ "not" , toJSON q ]
    toJSON (ResEqual flds val) = toJSON [ "=", toJSON flds, toJSON val ]
    toJSON (ResMatch flds val) = toJSON [ "~", toJSON flds, toJSON val ]

instance FromJSON ResourcesQuery3 where
    parseJSON (Array elems) = case V.toList elems of
                               ("or":xs)          -> ResOr    <$> mapM parseJSON xs
                               ("and":xs)         -> ResAnd   <$> mapM parseJSON xs
                               ["not",x]          -> ResNot   <$> parseJSON x
                               [ "=", flds, val ] -> ResEqual <$> parseJSON flds    <*> parseJSON val
                               [ "~", flds, val ] -> ResEqual <$> parseJSON flds    <*> parseJSON val
                               x -> fail ("unknown query" ++ show x)
    parseJSON _ = fail "Expected an array"
