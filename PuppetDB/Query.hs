module PuppetDB.Query where

import Data.List (intercalate)
import Data.Char (toUpper)
import Data.String.Utils (split)

data Operator = OEqual | OOver | OUnder | OOverE | OUnderE | OAnd | OOr | ONot
    deriving (Show, Ord, Eq)

-- [Field] Value
data Query = Query Operator [Query] | Term String | Terms [String]
    deriving (Show, Ord, Eq)

capitalize :: String -> String
capitalize = intercalate "::" . map capitalize' . split "::"

capitalize' :: String -> String
capitalize' "" = ""
capitalize' (x:xs) = toUpper x : xs

-- | Query used for realizing a resources, when its type and title is known.
queryRealize :: String -> String -> Query
queryRealize rtype rtitle = Query OAnd
                                [ Query OEqual [ Term "type",       Term (capitalize rtype) ]
                                , Query OEqual [ Term "title",      Term rtitle ]
                                , Query OEqual [ Term "exported",   Term "true" ]
                                ]

-- | Collects all resources of a given type
collectAll :: String -> Query
collectAll rtype = Query OAnd [ Query OEqual [ Term "type",     Term (capitalize rtype) ]
                              , Query OEqual [ Term "exported", Term "true" ]
                              ]

-- | Collections based on tags.
collectTag :: String -> String -> Query
collectTag rtype tagval = Query OAnd
                                [ Query OEqual [ Term "type",     Term (capitalize rtype) ]
                                , Query OEqual [ Term "tag" ,     Term tagval ]
                                , Query OEqual [ Term "exported", Term "true" ]
                                ]

-- | Used to emulate collections such as `Type<|| prmname == "prmval" ||>`
collectParam :: String -> String -> String -> Query
collectParam rtype prmname prmval = Query OAnd
                                    [ Query OEqual [ Term "type",                    Term (capitalize rtype) ]
                                    , Query OEqual [ Terms ["parameter", "prmname"], Term prmval ]
                                    , Query OEqual [ Term "exported",                Term "true" ]
                                    ]

dq :: String -> String
dq x = '"' : x ++ "\""

showOperator :: Operator -> String
showOperator OEqual     = dq "="
showOperator OOver      = dq ">"
showOperator OUnder     = dq "<"
showOperator OOverE     = dq ">="
showOperator OUnderE    = dq "<="
showOperator OAnd       = dq "and"
showOperator OOr        = dq "or"
showOperator ONot       = dq "not"

showQuery :: Query -> String
showQuery (Query op subqueries) = "[" ++ intercalate ", " (showOperator op : map showQuery subqueries) ++ "]"
showQuery (Term t)              = show t
showQuery (Terms ts)            = show ts
