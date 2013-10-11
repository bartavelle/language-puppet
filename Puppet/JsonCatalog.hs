module Puppet.JsonCatalog where

import Puppet.Parser.Types
import Puppet.Interpreter.Types

import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Aeson
import qualified Data.Vector as V
import Data.Attoparsec.Number
import Text.Parsec.Pos
import qualified Data.ByteString.Lazy as BSL
import Control.Arrow

prref = error "prref"

mkJsonCatalog :: T.Text -> Integer -> FinalCatalog -> FinalCatalog -> EdgeMap -> Value
mkJsonCatalog = error "mkJsonCatalog"

fakeResource :: (RIdentifier, PPosition) -> Resource
fakeResource (r,p) = Resource r (_iname r) HM.empty HM.empty ["fake"] ContRoot Normal (HS.singleton "fake") p

-- stuff that is done
-- * the EXPORTEDSOURCE is added for resources coming from PuppetDB
res2JSon :: Bool -> Resource -> Value
res2JSon = error "res2JSon"

rv2json :: PValue -> Value
rv2json (PString x) = String x
rv2json (PBoolean x) = Bool x
rv2json (PArray h) = Array (V.map rv2json h)
rv2json (PHash h) = Object $ HM.map rv2json h
rv2json _ = Null

catalog2JSon :: T.Text -> Integer -> FinalCatalog -> FinalCatalog -> EdgeMap -> BSL.ByteString
catalog2JSon nodename version dc de dm = encode (mkJsonCatalog nodename version dc de dm)
