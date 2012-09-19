module PuppetDB.Query where

import Data.List (intercalate)

data Operator = OEqual | OOver | OUnder | OOverE | OUnderE | OAnd | OOr | ONot
    deriving (Show, Ord, Eq)

-- [Field] Value
data Query = Query Operator [Query] | Term String | Terms [String]
    deriving (Show, Ord, Eq)

-- | Query used for realizing a resources, when its type and name is known.
queryRealize :: String -> String -> Query
queryRealize rtype rname = Query OAnd
                                [ Query OEqual [ Term "type", Term rtype ]
                                , Query OEqual [ Term "name", Term rname ]
                                ]

-- | Collects all resources of a given type
collectAll :: String -> Query
collectAll rtype = Query OAnd [ Query OEqual [ Term "type", Term rtype ] ]

-- | Collections based on tags.
collectTag :: String -> String -> Query
collectTag rtype tagval = Query OAnd
                                [ Query OEqual [ Term "type", Term rtype ]
                                , Query OEqual [ Term "tag" , Term tagval ]
                                ]

-- | Used to emulate collections such as `Type<|| prmname == "prmval" ||>`
collectParam :: String -> String -> String -> Query
collectParam rtype prmname prmval = Query OAnd
                                    [ Query OEqual [ Term "type", Term rtype ]
                                    , Query OEqual [ Terms ["parameter", "prmname"], Term prmval ]
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
