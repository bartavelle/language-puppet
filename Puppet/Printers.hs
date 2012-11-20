module Puppet.Printers (
    showRes
    , showRRes
    , showFCatalog
    , showValue
    , showRRef
    , capitalizeResType
) where

import Puppet.Interpreter.Types
import Puppet.DSL.Types
import qualified Data.Map as Map
import Data.List

showRes (CResource _ rname rtype params virtuality _) = putStrLn $ rtype ++ " " ++ show rname ++ " " ++ show params ++ " " ++ show virtuality
showRRes (RResource _ rname rtype params relations _) = putStrLn $ rtype ++ " " ++ show rname ++ " " ++ show params ++ " " ++ show relations

showFCatalog :: FinalCatalog -> String
showFCatalog rmap = let
    rawlist = groupBy (\((rtype1,_), _) ((rtype2,_), _) -> rtype1 == rtype2) (sort $ Map.toList rmap)
    rlist = map (map snd) rawlist
    out = concatMap showuniqueres rlist
    in out

-- helpers

commasep :: [String] -> String
commasep = intercalate ", "
commaretsep :: [String] -> String
commaretsep = intercalate ",\n"

showRRef :: ResIdentifier -> String
showRRef (rt, rn) = capitalizeResType rt ++ "[" ++ rn ++ "]"


showValue :: ResolvedValue -> String
showValue (ResolvedString x) = show x
showValue (ResolvedInt x) = show x
showValue (ResolvedDouble x) = show x
showValue (ResolvedBool True) = "true"
showValue (ResolvedBool False) = "false"
showValue (ResolvedRReference rt rn) = showRRef (rt, showValue rn)
showValue (ResolvedArray ar) = "[" ++ commasep (map showValue ar) ++ "]"
showValue (ResolvedHash h) = "{" ++ commasep (map (\(k,v) -> k ++ " => " ++ showValue v) h) ++ "}"
showValue (ResolvedRegexp r) = "/" ++ r ++ "/"
showValue (ResolvedUndefined) = "undef"

showuniqueres :: [RResource] -> String
showuniqueres res = mrtype ++ " {\n" ++ concatMap showrres res ++ "}\n"
    where
        showrres (RResource _ rname _ params rels mpos)  =
            let relslist = map asparams rels
                groupedrels = Map.fromListWith (++) relslist :: Map.Map String [ResolvedValue]
                maybeArray (s,[a]) = (s,a)
                maybeArray (s, x ) = (s,ResolvedArray x)
                paramlist = (Map.toList $ Map.delete "title" params) ++ map maybeArray (Map.toList groupedrels) :: [(String, ResolvedValue)]
                maxlen    = maximum (map (length . fst) paramlist) :: Int
            in  "    " ++ show rname ++ ":" ++ " #" ++ show mpos ++ "\n"
                ++ commaretsep (map (showparams maxlen) paramlist) ++ ";\n"
        showparams  mxl (name, val) = "        " ++ name ++ replicate (mxl - length name) ' ' ++ " => " ++ showValue val
        asparams    (RBefore, (dtype, dname))    = ("before",    [ResolvedRReference dtype (ResolvedString dname)])
        asparams    (RNotify, (dtype, dname))    = ("notify",    [ResolvedRReference dtype (ResolvedString dname)])
        asparams    (RSubscribe, (dtype, dname)) = ("subscribe", [ResolvedRReference dtype (ResolvedString dname)])
        asparams    (RRequire, (dtype, dname))   = ("require",   [ResolvedRReference dtype (ResolvedString dname)])
        mrtype = rrtype (head res)
