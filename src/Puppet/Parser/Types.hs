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
   ResCollDecl(..)
   ) where

import           XPrelude            hiding (show)

import           Data.List.NonEmpty  (NonEmpty)
import qualified Data.Maybe.Strict   as S
import qualified Data.Text           as Text
import qualified Data.Vector         as V
import qualified GHC.Exts            as Exts
import           GHC.Show            (Show (..))

import           Puppet.Language



-- | /High Order lambdas/.
newtype LambdaFunc = LambdaFunc Text deriving (Eq, Show)

-- | Lambda block parameters:
type LambdaParameters = V.Vector LambdaParameter

data LambdaParameter
  = LambdaParam !(Maybe UDataType) !Text
  deriving (Eq, Show)

-- The description of the /higher level lambda/ call.
data HOLambdaCall
  = HOLambdaCall
  { _hoLambdaFunc       :: !LambdaFunc
  , _hoLambdaExpr       :: !(V.Vector Expression)
  , _hoLambdaParams     :: !LambdaParameters
  , _hoLambdaStatements :: !(V.Vector Statement)
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
  | UInterpolable !(V.Vector Expression) -- ^ A string that might contain variable references. The type should be refined at one point.
  | UUndef -- ^ Special token that is generated when parsing the @undef@ literal.
  | UResourceReference !Text !Expression -- ^ A Resource[reference]
  | UArray !(V.Vector Expression)
  | UHash !(V.Vector (Pair Expression Expression))
  | URegexp !CompRegex -- ^ The regular expression compilation is performed during parsing.
  | UVariableReference !Text
  | UFunctionCall !Text !(V.Vector Expression)
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
  | Lookup !Expression !Expression
  | Negate !Expression
  | ConditionalValue !Expression !(V.Vector (Pair SelectorCase Expression)) -- ^ All conditionals are stored in this format.
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

-- | Search expression inside collector `<| searchexpr |>`
data SearchExpression
  = EqualitySearch !Text !Expression
  | NonEqualitySearch !Text !Expression
  | AndSearch !SearchExpression !SearchExpression
  | OrSearch !SearchExpression !SearchExpression
  | AlwaysTrue
  deriving (Eq, Show)

data CollectorType
  = Collector
  | ExportedCollector
  deriving (Eq, Show)

data NodeDesc
  = NodeName !Text
  | NodeMatch !CompRegex
  | NodeDefault
  deriving (Show, Eq)


-- | Resource declaration:
--
-- @ file { mode => 755} @
data ResDecl = ResDecl !Text !Expression !(V.Vector AttributeDecl) !Virtuality !PPosition deriving (Eq, Show)

-- | Resource default:
--
-- @ File { mode => 755 } @
--
-- <https://docs.puppetlabs.com/puppet/latest/reference/lang_defaults.html#language:-resource-default-statements puppet reference>.
data ResDefaultDecl = ResDefaultDecl !Text !(V.Vector AttributeDecl) !PPosition deriving (Eq, Show)

-- | Resource override:
--
-- @ File['title'] { mode => 755} @
--
-- See <https://docs.puppetlabs.com/puppet/latest/reference/lang_resources_advanced.html#amending-attributes-with-a-resource-reference puppet reference>.
data ResOverrideDecl = ResOverrideDecl !Text !Expression !(V.Vector AttributeDecl) !PPosition deriving (Eq, Show)

-- | All types of conditional statements : @case@, @if@, ...
--
-- Stored as an ordered list of pair @ (condition, statements) @.
-- Interpreted as "if first cond is true, choose first statements, else take the next pair, check the condition ..."
data ConditionalDecl = ConditionalDecl !(V.Vector (Pair Expression (V.Vector Statement))) !PPosition deriving (Eq, Show)

data ClassDecl  = ClassDecl !Text !(V.Vector (Pair (Pair Text (S.Maybe UDataType)) (S.Maybe Expression))) !(S.Maybe Text) !(V.Vector Statement) !PPosition deriving (Eq, Show)
data DefineDecl = DefineDecl !Text !(V.Vector (Pair (Pair Text (S.Maybe UDataType)) (S.Maybe Expression))) !(V.Vector Statement) !PPosition deriving (Eq, Show)

-- | A node is a collection of statements + maybe an inherit node.
data NodeDecl = NodeDecl !NodeDesc !(V.Vector Statement) !(S.Maybe NodeDesc) !PPosition deriving (Eq, Show)

-- | @ $newvar = 'world' @
data VarAssignDecl
  = VarAssignDecl
  { _vadtype  :: Maybe UDataType
  , _vadname  :: !Text
  , _vadvalue :: !Expression
  , _vadpos   :: !PPosition
  } deriving (Eq, Show)

data MainFuncDecl    = MainFuncDecl !Text !(V.Vector Expression) !PPosition deriving (Eq, Show)

-- | /Higher order function/ call.
data HigherOrderLambdaDecl = HigherOrderLambdaDecl !HOLambdaCall !PPosition deriving (Eq, Show)

-- | Resource Collector including exported collector (`\<\<| |>>`)
--
-- @ User \<| title == 'jenkins' |> { groups +> "docker"} @
--
-- See <https://docs.puppetlabs.com/puppet/latest/reference/lang_collectors.html#language:-resource-collectors puppet reference>
data ResCollDecl = ResCollDecl !CollectorType !Text !SearchExpression !(V.Vector AttributeDecl) !PPosition deriving (Eq, Show)

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
  | TopContainer !(V.Vector Statement) !Statement -- ^ Special statement used to include the expressions that are top level. Certainly buggy, but probably just like the original implementation.
  deriving (Eq, Show)

makeClassy ''HOLambdaCall
