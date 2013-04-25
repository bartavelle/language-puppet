module Puppet.Printers (
    showRes
    , showRRes
    , showFCatalog
    , showValue
    , showRRef
    , capitalizeResType
    , showScope
) where

import Puppet.Interpreter.Types
import Puppet.DSL.Types
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Puppet.Utils
import Data.List (groupBy,sort)

showRes (CResource _ rname rtype params virtuality _ _) = T.putStrLn $ rtype <> " " <> tshow rname <> " " <> tshow params <> " " <> tshow virtuality
showRRes (RResource _ rname rtype params relations _ _) = T.putStrLn $ rtype <> " " <> tshow rname <> " " <> tshow params <> " " <> tshow relations

showFCatalog :: FinalCatalog -> T.Text
showFCatalog rmap = let
    rawlist = groupBy (\((rtype1,_), _) ((rtype2,_), _) -> rtype1 == rtype2) (sort $ Map.toList rmap)
    rlist = map (map snd) rawlist
    out = T.concat$ map showuniqueres rlist
    in out

-- helpers

commasep :: [T.Text] -> T.Text
commasep = T.intercalate ", "
commaretsep :: [T.Text] -> T.Text
commaretsep = T.intercalate ",\n"

showRRef :: ResIdentifier -> T.Text
showRRef (rt, rn) = capitalizeResType rt <> "[" <> rn <> "]"


showValue :: ResolvedValue -> T.Text
showValue (ResolvedString x) = tshow x
showValue (ResolvedInt x) = tshow x
showValue (ResolvedDouble x) = tshow x
showValue (ResolvedBool True) = "True"
showValue (ResolvedBool False) = "False"
showValue (ResolvedRReference rt rn) = showRRef (rt, showValue rn)
showValue (ResolvedArray ar) = "[" <> commasep (map showValue ar) <> "]"
showValue (ResolvedHash h) = "{" <> commasep (map (\(k,v) -> k <> " => " <> showValue v) h) <> "}"
showValue (ResolvedRegexp r) = "/" <> r <> "/"
showValue (ResolvedUndefined) = "undef"

showuniqueres :: [RResource] -> T.Text
showuniqueres res = mrtype <> " {\n" <> T.concat (map showrres res) <> "}\n"
    where
        showrres (RResource _ rname _ params rels scopes mpos)  =
            let relslist = map asparams rels
                groupedrels = Map.fromListWith (<>) relslist :: Map.Map T.Text [ResolvedValue]
                maybeArray (s,[a]) = (s,a)
                maybeArray (s, x ) = (s,ResolvedArray x)
                paramlist = (Map.toList $ Map.delete "title" params) <> map maybeArray (Map.toList groupedrels) :: [(T.Text, ResolvedValue)]
                maxlen    = maximum (map (T.length . fst) paramlist) :: Int
            in  "    " <> tshow rname <> ":" <> " #" <> tshow mpos <> " " <> showScope scopes <> "\n"
                <> commaretsep (map (showparams maxlen) paramlist) <> ";\n"
        showparams  mxl (name, val) = "        " <> T.justifyLeft 15 ' ' name <> " => " <> showValue val
        asparams    (RBefore, (dtype, dname))    = ("before",    [ResolvedRReference dtype (ResolvedString dname)])
        asparams    (RNotify, (dtype, dname))    = ("notify",    [ResolvedRReference dtype (ResolvedString dname)])
        asparams    (RSubscribe, (dtype, dname)) = ("subscribe", [ResolvedRReference dtype (ResolvedString dname)])
        asparams    (RRequire, (dtype, dname))   = ("require",   [ResolvedRReference dtype (ResolvedString dname)])
        mrtype = rrtype (head res)

