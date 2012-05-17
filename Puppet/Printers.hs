module Puppet.Printers (
    showRes
    , showRRes
    , showFCatalog
) where

import Puppet.Interpreter.Types
import qualified Data.Map as Map
import Data.List

showRes (CResource crid rname rtype params relations virtuality pos) = putStrLn $ rtype ++ " " ++ show rname ++ " " ++ show params ++ " " ++ show relations ++ " " ++ show virtuality
showRRes (RResource crid rname rtype params relations pos) = putStrLn $ rtype ++ " " ++ show rname ++ " " ++ show params ++ " " ++ show relations

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

showValue :: ResolvedValue -> String
showValue (ResolvedString x) = show x
showValue (ResolvedInt x) = show x
showValue (ResolvedBool True) = "true"
showValue (ResolvedBool False) = "false"
showValue (ResolvedRReference rt rn) = rt ++ "[" ++ showValue rn ++ "]"
showValue (ResolvedArray ar) = "[" ++ commasep (map showValue ar) ++ "]"
showValue (ResolvedHash h) = "{" ++ commasep (map (\(k,v) -> k ++ " => " ++ showValue v) h) ++ "}"

showuniqueres :: [RResource] -> String
showuniqueres res = rtype ++ " {\n" ++ concatMap showrres res ++ "}\n"
    where
        showrres (RResource crid rname rtype params rels pos)  =
            "    " ++ show rname ++ ":" ++ " #" ++ show pos ++ "\n"
                ++ commaretsep (map showparams (sort params))
                ++ commareqs ((null rels) || (null params))
                ++ commaretsep (map showrequire (sort rels)) ++ ";\n"
        commareqs c | c                 = ""
                    | otherwise         = ",\n"
        showparams  (name, val)         = "        " ++ name ++ " => " ++ showValue val
        showrequire (ltype, src, dst)   = "        " ++ show ltype ++ " " ++ show dst
        rtype                           = rrtype (head res)
