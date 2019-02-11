{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
-- | All the types used for parsing, and helpers working on these types.
module Puppet.Parser.Types
 ( -- ** Expressions
   Expression(..),
   SelectorCase(..),
   UnresolvedValue(..),
   LambdaFunc(..),
   HOLambdaCall(..),
   ChainableRes(..),
   HasHOLambdaCall(..),
   LambdaParameter(..),
   LambdaParameters,
   CompRegex(..),
   CollectorType(..),
   Virtuality(..),
   NodeDesc(..),
   LinkType(..),
   -- ** Synonyms
   Parser,
   PuppetParseError,
   -- ** Datatypes
   UDataType(..),
   -- ** Search Expressions
   SearchExpression(..),
   -- ** Declaration
   AttributeDecl(..),
   ArrowOp(..),
   ConditionalDecl(..),
   ClassDecl(..),
   ResDefaultDecl(..),
   DepDecl(..),
   Statement(..),
   ResDecl(..),
   ResOverrideDecl(..),
   DefineDecl(..),
   NodeDecl(..),
   VarAssignDecl(..),
   MainFuncDecl(..),
   HigherOrderLambdaDecl(..),
   ResCollDecl(..),
   Parameters
   ) where

import           XPrelude           hiding (show)

import           Data.List.NonEmpty (NonEmpty)
import qualified Data.Maybe.Strict  as S
import qualified Data.Text          as Text
import qualified Data.Vector        as V
import qualified GHC.Exts           as Exts
import           GHC.Show           (Show (..))
import           Text.Megaparsec

import           Puppet.Language


type PuppetParseError = ParseError Char Void
type Parser = Parsec Void Text

-- | /High Order lambdas/.
newtype LambdaFunc = LambdaFunc Text deriving (Eq, Show)

-- | Lambda block parameters:
type LambdaParameters = Vector LambdaParameter

data LambdaParameter
  = LambdaParam !(Maybe UDataType) !Text
  deriving (Eq, Show)

-- The description of the /higher level lambda/ call.
data HOLambdaCall
  = HOLambdaCall
  { _hoLambdaFunc       :: !LambdaFunc
  , _hoLambdaExpr       :: !(Vector Expression)
  , _hoLambdaParams     :: !LambdaParameters
  , _hoLambdaStatements :: !(Vector Statement)
  , _hoLambdaLastExpr   :: !(S.Maybe Expression)
  } deriving (Eq,Show)

data ChainableRes
  = ChainResColl !ResCollDecl
  | ChainResDecl !ResDecl
  | ChainResRefr !Text [Expression] !PPosition
  deriving (Show, Eq)

data AttributeDecl
  = AttributeDecl !Text !ArrowOp !Expression
  | AttributeWildcard !Expression
  deriving (Show, Eq)

data ArrowOp
  = AppendArrow -- ^ `+>`
  | AssignArrow -- ^ `=>`
  deriving (Show, Eq)

-- | An unresolved value, typically the parser's output.
data UnresolvedValue
  = UBoolean !Bool -- ^ Special tokens generated when parsing the @true@ or @false@ literals.
  | UString !Text -- ^ Raw string.
  | UInterpolable !(Vector Expression) -- ^ A string that might contain variable references. The type should be refined at one point.
  | UUndef -- ^ Special token that is generated when parsing the @undef@ literal.
  | UResourceReference !Text !Expression -- ^ Alike @Resource[reference]@
  | UArray !(Vector Expression)
  | UHash !(Vector (Pair Expression Expression))
  | URegexp !CompRegex -- ^ The regular expression compilation is performed during parsing.
  | UVariableReference !Text
  | UFunctionCall !Text !(Vector Expression)
  | UHOLambdaCall !HOLambdaCall
  | UNumber !Scientific
  | UDataType UDataType
  deriving (Show, Eq)

instance Exts.IsList UnresolvedValue where
  type Item UnresolvedValue  = Expression
  fromList = UArray . V.fromList
  toList u = case u of
    UArray lst -> V.toList lst
    _          -> [Terminal u]

instance IsString UnresolvedValue where
  fromString = UString . Text.pack

data SelectorCase
  = SelectorValue !UnresolvedValue
  | SelectorType !UDataType
  | SelectorDefault
  deriving (Eq, Show)

-- | The 'Expression's
data Expression
  = Equal !Expression !Expression
  | Different !Expression !Expression
  | Not !Expression
  | And !Expression !Expression
  | Or !Expression !Expression
  | LessThan !Expression !Expression
  | MoreThan !Expression !Expression
  | LessEqualThan !Expression !Expression
  | MoreEqualThan !Expression !Expression
  | RegexMatch !Expression !Expression
  | NotRegexMatch !Expression !Expression
  | Contains !Expression !Expression
  | Addition !Expression !Expression
  | Substraction !Expression !Expression
  | Division !Expression !Expression
  | Multiplication !Expression !Expression
  | Modulo !Expression !Expression
  | RightShift !Expression !Expression
  | LeftShift !Expression !Expression
  | Lookup !Expression !Expression -- ^ Hash lookup @$var[\'key0\'][\'key1\']@
  | Negate !Expression
  | ConditionalValue !Expression !(Vector (Pair SelectorCase Expression)) -- ^ All conditionals are stored in this format.
  | FunctionApplication !Expression !Expression -- ^ This is for /higher order functions/.
  | Terminal !UnresolvedValue -- ^ Terminal object contains no expression
  deriving (Eq, Show)

data UDataType
  = UDTType
  | UDTString (Maybe Int) (Maybe Int)
  | UDTInteger (Maybe Int) (Maybe Int)
  | UDTFloat (Maybe Double) (Maybe Double)
  | UDTBoolean
  | UDTArray UDataType Int (Maybe Int)
  | UDTHash UDataType UDataType Int (Maybe Int)
  | UDTUndef
  | UDTScalar
  | UDTData
  | UDTOptional UDataType
  | UNotUndef
  | UDTVariant (NonEmpty UDataType)
  | UDTPattern (NonEmpty CompRegex)
  | UDTEnum (NonEmpty Expression)
  | UDTAny
  | UDTCollection
  | UDTRegexp (Maybe CompRegex)
  -- Tuple (NonEmpty DataType) Integer Integer
  -- DTDefault
  -- Struct TODO
  deriving (Eq, Show)

instance Exts.IsList Expression where
  type Item Expression = Expression
  fromList = Terminal . Exts.fromList
  toList u = case u of
    Terminal t -> Exts.toList t
    _          -> [u]

instance Num Expression where
  (+) = Addition
  (-) = Substraction
  (*) = Multiplication
  fromInteger = Terminal . UNumber . fromInteger
  abs x = ConditionalValue (MoreEqualThan x 0) (V.fromList [SelectorValue (UBoolean True) :!: x, SelectorDefault :!: negate x])
  signum x = ConditionalValue (MoreThan x 0) (V.fromList [SelectorValue (UBoolean True) :!: 1, SelectorDefault :!:
                                                         ConditionalValue (Equal x 0) (V.fromList [SelectorValue (UBoolean True) :!: 0, SelectorDefault :!: (-1)])
                                                         ])

instance Fractional Expression where
  (/) = Division
  recip x = 1 / x
  fromRational = Terminal . UNumber . fromRational

instance IsString Expression where
  fromString = Terminal . fromString

-- | Search expression inside collector @ \<| searchexpr |> @
data SearchExpression
  = EqualitySearch !Text !Expression
  | NonEqualitySearch !Text !Expression
  | AndSearch !SearchExpression !SearchExpression
  | OrSearch !SearchExpression !SearchExpression
  | AlwaysTrue
  deriving (Eq, Show)

data CollectorType
  = Collector -- ^ Single angle brackets @\<|   |>@
  | ExportedCollector -- ^ Double angle brackets @\<\<|   |>>@
  deriving (Eq, Show)

data NodeDesc
  = NodeName !Text
  | NodeMatch !CompRegex
  | NodeDefault
  deriving (Show, Eq)


-- | Resource declaration:
--
-- @ file { mode => 755} @
data ResDecl = ResDecl !Text !Expression !(Vector AttributeDecl) !Virtuality !PPosition deriving (Eq, Show)

-- | Resource default:
--
-- @ File { mode => 755 } @
--
-- <https://docs.puppetlabs.com/puppet/latest/reference/lang_defaults.html#language:-resource-default-statements puppet reference>.
data ResDefaultDecl = ResDefaultDecl !Text !(Vector AttributeDecl) !PPosition deriving (Eq, Show)

-- | Resource override:
--
-- @ File['title'] { mode => 755} @
--
-- See <https://docs.puppetlabs.com/puppet/latest/reference/lang_resources_advanced.html#amending-attributes-with-a-resource-reference puppet reference>.
data ResOverrideDecl = ResOverrideDecl !Text !Expression !(Vector AttributeDecl) !PPosition deriving (Eq, Show)

-- | All types of conditional statements : @case@, @if@, ...
--
-- Stored as an ordered list of pair @ (condition, statements) @.
-- Interpreted as "if first cond is true, choose first statements, else take the next pair, check the condition ..."
data ConditionalDecl = ConditionalDecl !(Vector (Pair Expression (Vector Statement))) !PPosition deriving (Eq, Show)

-- | Declare a class with
--
-- * a name
-- * a list of parameters
-- * an optional inherits
-- * a list of statements
-- * a position
data ClassDecl  = ClassDecl !Text  !Parameters !(S.Maybe Text) !(Vector Statement) !PPosition deriving (Eq, Show)

-- | Declare a define with
-- * a name
-- * a list of parameters
-- * a list of statements
-- * a position
data DefineDecl = DefineDecl !Text !Parameters !(Vector Statement) !PPosition deriving (Eq, Show)

type Parameters = Vector (Pair (Pair Text (S.Maybe UDataType)) (S.Maybe Expression))

-- | A node is a collection of statements + maybe an inherit node.
data NodeDecl = NodeDecl !NodeDesc !(Vector Statement) !(S.Maybe NodeDesc) !PPosition deriving (Eq, Show)

-- | @ $newvar = 'world' @
data VarAssignDecl
  = VarAssignDecl
  { _vadtype  :: Maybe UDataType
  , _vadnames  :: [Text]
  , _vadvalue :: !Expression
  , _vadpos   :: !PPosition
  } deriving (Eq, Show)

data MainFuncDecl    = MainFuncDecl !Text !(Vector Expression) !PPosition deriving (Eq, Show)

-- | /Higher order function/ call.
data HigherOrderLambdaDecl = HigherOrderLambdaDecl !HOLambdaCall !PPosition deriving (Eq, Show)

-- | Resource Collector including exported collector (`\<\<| |>>`)
--
-- @ User \<| title == 'jenkins' |> { groups +> "docker"} @
--
-- See <https://docs.puppetlabs.com/puppet/latest/reference/lang_collectors.html#language:-resource-collectors puppet reference>
data ResCollDecl = ResCollDecl !CollectorType !Text !SearchExpression !(Vector AttributeDecl) !PPosition deriving (Eq, Show)

data DepDecl = DepDecl !(Pair Text Expression) !(Pair Text Expression) !LinkType !PPosition deriving (Eq, Show)

-- | All possible statements.
data Statement
  = ResourceDeclaration !ResDecl
  | ResourceDefaultDeclaration !ResDefaultDecl
  | ResourceOverrideDeclaration !ResOverrideDecl
  | ResourceCollectionDeclaration !ResCollDecl
  | ClassDeclaration !ClassDecl
  | DefineDeclaration !DefineDecl
  | NodeDeclaration !NodeDecl
  | ConditionalDeclaration !ConditionalDecl
  | VarAssignmentDeclaration !VarAssignDecl
  | MainFunctionDeclaration !MainFuncDecl
  | HigherOrderLambdaDeclaration !HigherOrderLambdaDecl
  | DependencyDeclaration !DepDecl
  | TopContainer !(Vector Statement) !Statement -- ^ Special statement used to include the expressions that are top level. Certainly buggy, but probably just like the original implementation.
  deriving (Eq, Show)

makeClassy ''HOLambdaCall
