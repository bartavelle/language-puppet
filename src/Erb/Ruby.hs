-- | Base types for the internal ruby parser ("Erb.Parser").
module Erb.Ruby where

import           XPrelude

data Value
    = Literal !Text
    | Interpolable ![Expression]
    | Symbol !Text
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
    | BlockOperation !Text
    | Value !Value
    | BTrue
    | BFalse
    | Error !String
    deriving (Show, Ord, Eq)

instance Pretty Expression where
    pretty (LookupOperation a b) = pretty a <> brackets (pretty b)
    pretty (PlusOperation a b) = parens (pretty a <+> "+" <+> pretty b)
    pretty (MinusOperation a b) = parens (pretty a <+> "-" <+> pretty b)
    pretty (DivOperation a b) = parens (pretty a <+> "/" <+> pretty b)
    pretty (MultiplyOperation a b) = parens (pretty a <+> "*" <+> pretty b)
    pretty op = ppline (show op)

data RubyStatement
    = Puts !Expression
    | DropPrevSpace !RubyStatement
    | DropPrevSpace'
    | DropNextSpace !RubyStatement
    deriving(Show,Eq)
