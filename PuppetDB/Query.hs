module PuppetDB.Query where

data QueryType = QAnd | QOr | QNot
    deriving (Show, Ord, Eq)

data Operator = OEqual | OOver | OUnder | OOverE | OUnderE | OAnd | OOr | ONot
    deriving (Show, Ord, Eq)

-- [Field] Value
data Query = Query Operator [Query] | Term String
    deriving (Show, Ord, Eq)

-- | Query used for realizing a resources, when its type and name is known.
queryRealize :: String -> String -> Query
queryRealize rtype rname = Query OAnd
                                [ Query OEqual [ Term "type", Term rtype ]
                                , Query OEqual [ Term "name", Term rname ]
                                ]

showQuery :: Query -> String
showQuery = show
