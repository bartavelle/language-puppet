module PuppetDB.Query where

import Puppet.DSL.Types
import qualified Data.Text as T
import Data.Monoid
import Puppet.Utils

data Operator = OEqual | OOver | OUnder | OOverE | OUnderE | OAnd | OOr | ONot
    deriving (Show, Ord, Eq)

-- [Field] Value
data Query = Query Operator [Query] | Term T.Text | Terms [T.Text]
    deriving (Show, Ord, Eq)

-- | Query used for realizing a resources, when its type and title is known.
queryRealize :: T.Text -> T.Text -> Query
queryRealize rtype rtitle = Query OAnd
                                [ Query OEqual [ Term "type",       Term (capitalizeResType rtype) ]
                                , Query OEqual [ Term "title",      Term rtitle ]
                                , Query OEqual [ Term "exported",   Term "true" ]
                                ]

-- | Collects all resources of a given type
collectAll :: T.Text -> Query
collectAll rtype = Query OAnd [ Query OEqual [ Term "type",     Term (capitalizeResType rtype) ]
                              , Query OEqual [ Term "exported", Term "true" ]
                              ]

-- | Collections based on tags.
collectTag :: T.Text -> T.Text -> Query
collectTag rtype tagval = Query OAnd
                                [ Query OEqual [ Term "type",     Term (capitalizeResType rtype) ]
                                , Query OEqual [ Term "tag" ,     Term tagval ]
                                , Query OEqual [ Term "exported", Term "true" ]
                                ]

-- | Used to emulate collections such as `Type<|| prmname == "prmval" ||>`
collectParam :: T.Text -> T.Text -> T.Text -> Query
collectParam rtype prmname prmval = Query OAnd
                                    [ Query OEqual [ Term "type",                    Term (capitalizeResType rtype) ]
                                    , Query OEqual [ Terms ["parameter", "prmname"], Term prmval ]
                                    , Query OEqual [ Term "exported",                Term "true" ]
                                    ]

showOperator :: Operator -> T.Text
showOperator OEqual     = dq "="
showOperator OOver      = dq ">"
showOperator OUnder     = dq "<"
showOperator OOverE     = dq ">="
showOperator OUnderE    = dq "<="
showOperator OAnd       = dq "and"
showOperator OOr        = dq "or"
showOperator ONot       = dq "not"

getOperator :: T.Text -> Maybe Operator
getOperator "="   = Just OEqual
getOperator ">"   = Just OOver
getOperator "<"   = Just OUnder
getOperator ">="  = Just OOverE
getOperator "<="  = Just OUnderE
getOperator "and" = Just OAnd
getOperator "or"  = Just OOr
getOperator "not" = Just ONot
getOperator _ = Nothing

showQuery :: Query -> T.Text
showQuery (Query op subqueries) = "[" <> T.intercalate ", " (showOperator op : map showQuery subqueries) <> "]"
showQuery (Term t)              = tshow t
showQuery (Terms ts)            = tshow ts

