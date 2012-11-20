{-# LANGUAGE OverloadedStrings #-}
module Puppet.JsonCatalog where

import Puppet.DSL.Types hiding (Value)
import Puppet.Interpreter.Types
import Puppet.Printers

import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as Map
import Data.Aeson
import qualified Data.Vector as V
import Data.Attoparsec.Number
import Text.Parsec.Pos
import qualified Data.ByteString.Lazy as BSL

prref = String . T.pack . showRRef

mkJsonCatalog :: T.Text -> Integer -> FinalCatalog -> FinalCatalog -> EdgeMap -> Value
mkJsonCatalog nodename version cat exports edges = Object $ HM.fromList [("data",datahash), ("document_type", String "Catalog"), ("metadata", Object (HM.fromList [("api_version", Number 1)]))]
    where
        datahash = Object $ HM.fromList [ ("classes"    , Array (V.fromList classes))
                                        , ("edges"      , Array (V.fromList ledges))
                                        , ("environment", String "production")
                                        , ("name"       , String nodename)
                                        , ("resources"  , Array (V.fromList resources))
                                        , ("tags"       , Array V.empty)
                                        , ("version"    , Number (I version))
                                        ]
        lcat = Map.toList cat
        classes = map (String . T.pack . snd . fst) . filter (\((k,_),_) -> k == "class") $ lcat
        --notcatalog = map fakeResource $ nubBy (\(a,_) (b,_) -> a == b) .  filter (\(x,_) -> not (Map.member x cat)) . concatMap (\((a,b),(_,_,c)) -> [(a,c),(b,c)]) . Map.toList $ edges
        ledges = map (\(s,d) -> Object $ HM.fromList [("source", prref s),("target", prref d)] ) . filter (\i -> Map.member (fst i) cat && Map.member (snd i) cat) . Map.keys $ edges
        resources = map (res2JSon False . snd) lcat

fakeResource :: (ResIdentifier, SourcePos) -> RResource
fakeResource ((t,n),p) = RResource 0 n t Map.empty [] p

-- stuff that is done
-- * the EXPORTEDSOURCE is added for resources coming from PuppetDB
res2JSon :: Bool -> RResource -> Value
res2JSon isExported (RResource _ rn rt rp _ rpos) = Object $ HM.fromList [ ("exported", Bool isExported)
                                                                         , ("file", String (T.pack (sourceName rpos)))
                                                                         , ("line", Number (fromIntegral (sourceLine rpos)))
                                                                         , ("parameters", Object (HM.delete "EXPORTEDSOURCE" $ HM.fromList paramlist))
                                                                         , ("tags",  Array V.empty)
                                                                         , ("title", String (T.pack realtitle))
                                                                         , ("type", String . T.pack . capitalizeResType $ rt)
                                                                         ]
    where
        -- in puppet class titles are capitalized ...
        realtitle = if rt == "class"
                        then capitalizeResType ctitle
                        else ctitle
        ctitle = case Map.lookup "title" rp of
                     Just (ResolvedString s) -> s
                     _ -> rn
        paramlist = map (\(k,v) -> (T.pack k, rv2json v)) $ Map.toList $ Map.delete "title" rp

rv2json :: ResolvedValue -> Value
rv2json (ResolvedString x) = String (T.pack x)
rv2json (ResolvedRegexp x) = String (T.pack x)
rv2json (ResolvedInt x) = Number (I x)
rv2json (ResolvedDouble x) = Number (D x)
rv2json (ResolvedBool x) = Bool x
rv2json (ResolvedArray h) = Array (V.fromList (map rv2json h))
rv2json (ResolvedHash h) = Object $ HM.fromList $ map (\(k,v) -> (T.pack k, rv2json v)) h
rv2json _ = Null

catalog2JSon :: String -> Integer -> FinalCatalog -> FinalCatalog -> EdgeMap -> BSL.ByteString
catalog2JSon nodename version dc de dm = encode (mkJsonCatalog (T.pack nodename) version dc de dm)
