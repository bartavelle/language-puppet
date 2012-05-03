module Puppet.DSL.Types where

import Text.Parsec.Pos

data Parameters = Parameters [(Expression, Expression)] deriving(Show, Ord, Eq)

data TopLevelType = TopNode | TopDefine | TopClass deriving (Show, Ord, Eq)
convertTopLevel :: Statement -> Either Statement (TopLevelType, String, Statement)
convertTopLevel x@(Node name _ _)                 = Right (TopNode, name, x)
convertTopLevel x@(ClassDeclaration name _ _ _ _) = Right (TopClass, name, x)
convertTopLevel x@(DefineDeclaration name _ _ _)  = Right (TopDefine, name, x)
convertTopLevel x                                 = Left x

-- type, name
data Value
    = Literal String
    | Interpolable [Value]
    | PuppetRegexp String
    | Double Double
    | Integer Integer
    | VariableReference String
    | Empty
    | ResourceReference String Expression -- restype resname
    | PuppetArray [Expression]
    | PuppetHash Parameters
    | FunctionCall String [Expression]
    | Undefined
    deriving(Show, Ord, Eq)

data Virtuality = Normal | Virtual | Exported deriving(Show, Ord, Eq)

data Statement
    = Node String [Statement] SourcePos
    | VariableAssignment String Expression SourcePos
    | Include String SourcePos
    | Require String SourcePos
    | Resource String Expression [(Expression, Expression)] Virtuality SourcePos -- type name params
    | ResourceDefault String [(Expression, Expression)] SourcePos -- type params
    | ResourceOverride String Expression [(Expression, Expression)] SourcePos -- type name params
    | ConditionalStatement [(Expression, [Statement])] SourcePos
    | ClassDeclaration String (Maybe String) [(String, Maybe Expression)] [Statement] SourcePos -- nom, heritage, parametres, contenu
    | DefineDeclaration String [(String, Maybe Expression)] [Statement] SourcePos -- nom, parametres, contenu
    | ResourceCollection String Expression [(Expression, Expression)] SourcePos
    | VirtualResourceCollection String Expression [(Expression, Expression)] SourcePos
    | DependenceChain (String,Expression) (String,Expression) SourcePos
    | MainFunctionCall String [Expression] SourcePos
    deriving(Show, Ord, Eq)


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
    | Value Value
	| ResolvedResourceReference String String
    | BTrue
    | BFalse
    | Error String
    deriving(Show, Ord, Eq)
