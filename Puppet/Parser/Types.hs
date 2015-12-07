{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
-- | All the types used for parsing, and helpers working on these types.
module Puppet.Parser.Types
 ( -- * Position management
   Position,
   PPosition,
   initialPPos,
   toPPos,
   -- ** Lenses
   lSourceName,
   lSourceLine,
   lSourceColumn,
   -- * Helpers
   capitalizeRT,
   rel2text,
   -- * Types
   -- ** Expressions
   Expression(..),
   SelectorCase(..),
   UnresolvedValue(..),
   LambdaFunc(..),
   HOLambdaCall(..),
   ChainableRes(..),
   HasHOLambdaCall(..),
   LambdaParameters(..),
   CompRegex(..),
   CollectorType(..),
   Virtuality(..),
   NodeDesc(..),
   LinkType(..),
   -- ** Search Expressions
   SearchExpression(..),
   -- ** Statements
   ArrowOp(..),
   AttributeDecl(..),
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

import           Control.Lens
import           Data.Aeson
import           Data.Char              (toUpper)
import           Data.Hashable
import qualified Data.Maybe.Strict      as S
import           Data.Scientific
import           Data.String
import           Data.Text             (Text)
import qualified Data.Text              as T
import           Data.Tuple.Strict
import qualified Data.Vector            as V

import           GHC.Generics

import           Text.Megaparsec.Pos
import           Text.Regex.PCRE.String

-- | Properly capitalizes resource types
capitalizeRT :: Text -> Text
capitalizeRT = T.intercalate "::" . map capitalize' . T.splitOn "::"
    where
        capitalize' :: Text -> Text
        capitalize' t | T.null t = T.empty
                      | otherwise = T.cons (toUpper (T.head t)) (T.tail t)

-- | A pair containing the start and end of a given token.
type PPosition = Pair Position Position

-- | Position in a puppet file. Currently an alias to 'SourcePos'.
type Position = SourcePos

lSourceName :: Lens' Position String
lSourceName = lens sourceName setSourceName

lSourceLine :: Lens' Position Int
lSourceLine = lens sourceLine setSourceLine

lSourceColumn :: Lens' Position Int
lSourceColumn = lens sourceColumn setSourceColumn

-- | Generates an initial position based on a filename.
initialPPos :: Text -> PPosition
initialPPos x =
    let i = initialPos (T.unpack x)
    in (i :!: i)

-- | Generates a 'PPosition' based on a filename and line number.
toPPos :: Text -> Int -> PPosition
toPPos fl ln =
    let p = newPos (T.unpack fl) ln (-1)
    in  (p :!: p)

-- | High Order "lambdas"
data LambdaFunc
    = LambEach
    | LambMap
    | LambReduce
    | LambFilter
    | LambSlice
    deriving (Eq, Show)

-- | Lambda block parameters:
-- Currently only two types of block parameters are supported:
-- single values and pairs.
data LambdaParameters
    = BPSingle !Text -- ^ @|k|@
    | BPPair   !Text !Text -- ^ @|k,v|@
    deriving (Eq, Show)

-- The description of the /higher level lambda/ call.
data HOLambdaCall = HOLambdaCall
    { _hoLambdaFunc       :: !LambdaFunc
    , _hoLambdaExpr       :: !(S.Maybe Expression)
    , _hoLambdaParams     :: !LambdaParameters
    , _hoLambdaStatements :: !(V.Vector Statement)
    , _hoLambdaLastExpr   :: !(S.Maybe Expression)
    } deriving (Eq,Show)

data ChainableRes
    = ChainResColl !ResCollDecl
    | ChainResDecl !ResDecl
    | ChainResRefr !Text [Expression] !PPosition
    deriving (Show, Eq)

data AttributeDecl = AttributeDecl !Text !ArrowOp !Expression
    deriving (Show, Eq)
data ArrowOp
    = AppendArrow -- ^ `+>`
    | AssignArrow -- ^ `=>`
    deriving (Show, Eq)

data CompRegex = CompRegex !Text !Regex
instance Show CompRegex where
  show (CompRegex t _) = show t
instance Eq CompRegex where
    (CompRegex a _) == (CompRegex b _) = a == b

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
    deriving (Show, Eq)


instance IsString UnresolvedValue where
    fromString = UString . T.pack

data SelectorCase
    = SelectorValue !UnresolvedValue
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

data Virtuality
    = Normal -- ^ Normal resource, that will be included in the catalog
    | Virtual -- ^ Type for virtual resources
    | Exported -- ^ Type for exported resources
    | ExportedRealized -- ^ These are resources that are exported AND included in the catalogderiving (Generic, Eq, Show)
    deriving (Eq, Show)

data NodeDesc
    = NodeName !Text
    | NodeMatch !CompRegex
    | NodeDefault
    deriving (Show, Eq)

-- | Relationship link type.
data LinkType
    = RNotify
    | RRequire
    | RBefore
    | RSubscribe
    deriving(Show, Eq,Generic)
instance Hashable LinkType

rel2text :: LinkType -> Text
rel2text RNotify = "notify"
rel2text RRequire = "require"
rel2text RBefore = "before"
rel2text RSubscribe = "subscribe"

instance FromJSON LinkType where
    parseJSON (String "require")   = return RRequire
    parseJSON (String "notify")    = return RNotify
    parseJSON (String "subscribe") = return RSubscribe
    parseJSON (String "before")    = return RBefore
    parseJSON _ = fail "invalid linktype"

instance ToJSON LinkType where
    toJSON = String . rel2text

data ResDecl         = ResDecl !Text !Expression !(V.Vector AttributeDecl) !Virtuality !PPosition deriving (Eq, Show)
data ResDefaultDecl  = ResDefaultDecl !Text !(V.Vector AttributeDecl) !PPosition deriving (Eq, Show)
data ResOverrideDecl = ResOverrideDecl !Text !Expression !(V.Vector AttributeDecl) !PPosition deriving (Eq, Show)
-- | All types of conditional statements (@case@, @if@, etc.) are stored as an ordered list of pair (condition, statements)
-- (interpreted as "if first cond is true, choose first statements, else take the next pair, check the condition ...")
data ConditionalDecl = ConditionalDecl !(V.Vector (Pair Expression (V.Vector Statement))) !PPosition deriving (Eq, Show)
data ClassDecl       = ClassDecl !Text !(V.Vector (Pair Text (S.Maybe Expression))) !(S.Maybe Text) !(V.Vector Statement) !PPosition deriving (Eq, Show)
data DefineDecl      = DefineDecl !Text !(V.Vector (Pair Text (S.Maybe Expression))) !(V.Vector Statement) !PPosition deriving (Eq, Show)
-- | A node is a collection of statements + maybe an inherit node
data NodeDecl        = NodeDecl !NodeDesc !(V.Vector Statement) !(S.Maybe NodeDesc) !PPosition deriving (Eq, Show)
data VarAssignDecl       = VarAssignDecl !Text !Expression !PPosition deriving (Eq, Show)
data MainFuncDecl    = MainFuncDecl !Text !(V.Vector Expression) !PPosition deriving (Eq, Show)
-- | /Higher order function/ call.
data HigherOrderLambdaDecl   = HigherOrderLambdaDecl !HOLambdaCall !PPosition deriving (Eq, Show)
-- | For all types of collectors.
data ResCollDecl     = ResCollDecl !CollectorType !Text !SearchExpression !(V.Vector AttributeDecl) !PPosition deriving (Eq, Show)
data DepDecl         = DepDecl !(Pair Text Expression) !(Pair Text Expression) !LinkType !PPosition deriving (Eq, Show)

-- | All the possible statements
data Statement
    = ResourceDeclaration !ResDecl                 -- ^ e.g `file { mode => 755}`
    | ResourceDefaultDeclaration !ResDefaultDecl   -- ^ Resource default e.g `File { mode => 755 }`
    | ResourceOverrideDeclaration !ResOverrideDecl -- ^ e.g `File['title'] { mode => 755}`
    | ResourceCollectionDeclaration !ResCollDecl   -- ^ e.g `User <| title == 'jenkins' |> { groups +> "docker"}`
    | ClassDeclaration !ClassDecl
    | DefineDeclaration !DefineDecl
    | NodeDeclaration !NodeDecl
    | ConditionalDeclaration !ConditionalDecl
    | VarAssignmentDeclaration !VarAssignDecl      -- ^ e.g $newvar = 'world'
    | MainFunctionDeclaration !MainFuncDecl
    | HigherOrderLambdaDeclaration !HigherOrderLambdaDecl
    | DependencyDeclaration !DepDecl
    | TopContainer !(V.Vector Statement) !Statement -- ^ Special statement used to include the expressions that are top level. Certainly buggy, but probably just like the original implementation.
    deriving (Eq, Show)

makeClassy ''HOLambdaCall
