module Erb.Ruby where

import qualified Data.Text as T
import Text.PrettyPrint.ANSI.Leijen

data Value
    = Literal !T.Text
    | Array ![Expression]
    deriving (Show, Ord, Eq)

data Expression
    = LookupOperation !Expression !Expression
    | PlusOperation !Expression !Expression
    | MinusOperation !Expression !Expression
    | DivOperation !Expression !Expression
    | MultiplyOperation !Expression !Expression
    | ShiftLeftOperation !Expression !Expression
    | ShiftRightOperation !Expression !Expression
    | AndOperation !Expression !Expression
    | OrOperation !Expression !Expression
    | EqualOperation !Expression !Expression
    | DifferentOperation !Expression !Expression
    | AboveOperation !Expression !Expression
    | AboveEqualOperation !Expression !Expression
    | UnderEqualOperation !Expression !Expression
    | UnderOperation !Expression !Expression
    | RegexpOperation !Expression !Expression
    | NotRegexpOperation !Expression !Expression
    | NotOperation !Expression
    | NegOperation !Expression
    | ConditionalValue !Expression !Expression
    | Object !Expression
    | MethodCall !Expression !Expression
    | BlockOperation !T.Text
    | Value !Value
    | BTrue
    | BFalse
    | Error !String
    deriving (Show, Ord, Eq)

instance Pretty Expression where
    pretty (LookupOperation a b) = pretty a <> brackets (pretty b)
    pretty (PlusOperation a b) = parens (pretty a <+> text "+" <+> pretty b)
    pretty (MinusOperation a b) = parens (pretty a <+> text "-" <+> pretty b)
    pretty (DivOperation a b) = parens (pretty a <+> text "/" <+> pretty b)
    pretty (MultiplyOperation a b) = parens (pretty a <+> text "*" <+> pretty b)
    pretty op = text (show op)

data RubyStatement
    = Puts !Expression
    deriving(Show)

