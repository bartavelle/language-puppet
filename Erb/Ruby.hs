module Erb.Ruby where

data Value
    = Literal String
    deriving (Show, Ord, Eq)

data Expression
    = LookupOperation Expression Expression
    | PlusOperation Expression Expression
    | MinusOperation Expression Expression
    | DivOperation Expression Expression
    | MultiplyOperation Expression Expression
    | ShiftLeftOperation Expression Expression
    | ShiftRightOperation Expression Expression
    | AndOperation Expression Expression
    | OrOperation Expression Expression
    | EqualOperation Expression Expression
    | DifferentOperation Expression Expression
    | AboveOperation Expression Expression
    | AboveEqualOperation Expression Expression
    | UnderEqualOperation Expression Expression
    | UnderOperation Expression Expression
    | RegexpOperation Expression Expression
    | NotRegexpOperation Expression Expression
    | NotOperation Expression
    | NegOperation Expression
    | ConditionalValue Expression Expression
    | Object Expression
    | MethodCall Expression Expression
    | BlockOperation String
    | Value Value
    | BTrue
    | BFalse
    | Error String
    deriving (Show, Ord, Eq)

data RubyStatement
    = Puts Expression
    deriving(Show)
