{-# LANGUAGE DeriveGeneric, TemplateHaskell #-}
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
   capitalize',
   capitalizeRT,
   array,
   toBool,
   rel2text,
   -- * Types
   -- ** Expressions
   Expression(..),
   SelectorCase(..),
   UValue(..),
   HigherFuncType(..),
   HFunctionCall(..),
   HasHFunctionCall(..),
   BlockParameters(..),
   CollectorType(..),
   Virtuality(..),
   NodeDesc(..),
   LinkType(..),
   -- ** Search Expressions
   SearchExpression(..),
   -- ** Statements
   Statement(..),
   ResDec(..),
   DefaultDec(..),
   ResOver(..),
   CondStatement(..),
   ClassDecl(..),
   DefineDec(..),
   Nd(..),
   VarAss(..),
   MFC(..),
   SFC(..),
   RColl(..),
   Dep(..)
   ) where


import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Hashable
import Data.Tuple.Strict
import qualified Data.Maybe.Strict as S
import GHC.Generics
import Data.Char (toUpper)
import Text.Regex.PCRE.String
import Control.Lens
import Data.String
import Data.Scientific
import Data.Aeson

import Text.Parsec.Pos

-- | Properly capitalizes resource types
capitalizeRT :: T.Text -> T.Text
capitalizeRT = T.intercalate "::" . map capitalize' . T.splitOn "::"

capitalize' :: T.Text -> T.Text
capitalize' t | T.null t = T.empty
              | otherwise = T.cons (toUpper (T.head t)) (T.tail t)

-- | A pair containing the start and end of a given token.
type PPosition = Pair Position Position

-- | Position in a puppet file. Currently an alias to 'SourcePos'.
type Position = SourcePos

lSourceName :: Lens' Position SourceName
lSourceName = lens sourceName setSourceName

lSourceLine :: Lens' Position Line
lSourceLine = lens sourceLine setSourceLine

lSourceColumn :: Lens' Position Column
lSourceColumn = lens sourceColumn setSourceColumn

-- | Generates an initial position based on a filename.
initialPPos :: T.Text -> PPosition
initialPPos x =
    let i = initialPos (T.unpack x)
    in (i :!: i)

-- | Generates a 'PPosition' based on a filename and line number.
toPPos :: T.Text -> Int -> PPosition
toPPos fl ln =
    let p = newPos (T.unpack fl) ln (-1)
    in  (p :!: p)

-- | The distinct Puppet /higher order functions/
data HigherFuncType = HFEach
                    | HFMap
                    | HFReduce
                    | HFFilter
                    | HFSlice
                    deriving Eq

-- | Currently only two types of block parameters are supported, single
-- values and pairs.
data BlockParameters = BPSingle !T.Text -- ^ @|k|@
                     | BPPair   !T.Text !T.Text -- ^ @|k,v|@
                     deriving Eq

-- The description of the /higher level function/ call.
data HFunctionCall = HFunctionCall { _hftype       :: !HigherFuncType
                                   , _hfexpr       :: !(S.Maybe Expression)
                                   , _hfparams     :: !BlockParameters
                                   , _hfstatements :: !(V.Vector Statement)
                                   , _hfexpression :: !(S.Maybe Expression)
                                   }
                   deriving Eq

-- | An unresolved value, typically the parser's output.
data UValue
    = UBoolean !Bool -- ^ Special tokens generated when parsing the @true@ or @false@ literals.
    | UString !T.Text -- ^ Raw string.
    | UInterpolable !(V.Vector UValue) -- ^ A string that might contain variable references. The type should be refined at one point.
    | UUndef -- ^ Special token that is generated when parsing the @undef@ literal.
    | UResourceReference !T.Text !Expression -- ^ A Resource[reference]
    | UArray !(V.Vector Expression)
    | UHash !(V.Vector (Pair Expression Expression))
    | URegexp !T.Text !Regex -- ^ The regular expression compilation is performed during parsing.
    | UVariableReference !T.Text
    | UFunctionCall !T.Text !(V.Vector Expression)
    | UHFunctionCall !HFunctionCall
    | UNumber Scientific

instance IsString UValue where
    fromString = UString . T.pack

-- The Eq instance is manual, because of the 'Regex' comparison problem
instance Eq UValue where
    (==) (UBoolean a)               (UBoolean b)                = a == b
    (==) (UString a)                (UString b)                 = a == b
    (==) (UInterpolable a)          (UInterpolable b)           = a == b
    (==) UUndef                     UUndef                      = True
    (==) (UResourceReference a1 a2) (UResourceReference b1 b2)  = (a1 == b1) && (a2 == b2)
    (==) (UArray a)                 (UArray b)                  = a == b
    (==) (UHash a)                  (UHash b)                   = a == b
    (==) (URegexp a _)              (URegexp b _)               = a == b
    (==) (UVariableReference a)     (UVariableReference b)      = a == b
    (==) (UFunctionCall a1 a2)      (UFunctionCall b1 b2)       = (a1 == b1) && (a2 == b2)
    (==) (UNumber a)                (UNumber b)                 = a == b
    (==) _ _ = False

-- | A helper function for writing 'array's.
array :: [Expression] -> UValue
array = UArray . V.fromList

data SelectorCase = SelectorValue UValue
                  | SelectorDefault
                  deriving (Eq)

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
    | Terminal !UValue
    deriving Eq

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

data SearchExpression
    = EqualitySearch !T.Text !Expression
    | NonEqualitySearch !T.Text !Expression
    | AndSearch !SearchExpression !SearchExpression
    | OrSearch !SearchExpression !SearchExpression
    | AlwaysTrue
    deriving Eq

data CollectorType = Collector | ExportedCollector
    deriving (Eq)

-- | Tries to turn an unresolved value into a 'Bool' without any context.
toBool :: UValue -> Bool
toBool (UString "")      = False
toBool (UInterpolable v) = not (V.null v)
toBool UUndef            = False
toBool (UBoolean x)      = x
toBool _                 = True

data Virtuality = Normal -- ^ Normal resource, that will be included in the catalog
                | Virtual -- ^ Type for virtual resources
                | Exported -- ^ Type for exported resources
                | ExportedRealized -- ^ These are resources that are exported AND included in the catalog
    deriving (Generic, Eq)

data NodeDesc = NodeName !T.Text
              | NodeMatch !T.Text !Regex
              | NodeDefault

instance Eq NodeDesc where
    (==) (NodeName a) (NodeName b) = a == b
    (==) NodeDefault NodeDefault = True
    (==) (NodeMatch a _) (NodeMatch b _) = a == b
    (==) _ _ = False


-- | Relationship link type.
data LinkType = RNotify | RRequire | RBefore | RSubscribe deriving(Show, Eq,Generic)
instance Hashable LinkType

rel2text :: LinkType -> T.Text
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

data ResDec        = ResDec !T.Text !Expression !(V.Vector (Pair T.Text Expression)) !Virtuality !PPosition deriving Eq
data DefaultDec    = DefaultDec !T.Text !(V.Vector (Pair T.Text Expression)) !PPosition deriving Eq
data ResOver       = ResOver !T.Text !Expression !(V.Vector (Pair T.Text Expression)) !PPosition deriving Eq
data CondStatement = CondStatement !(V.Vector (Pair Expression (V.Vector Statement))) !PPosition deriving Eq -- ^ All types of conditional statements are stored that way (@case@, @if@, etc.)
data ClassDecl     = ClassDecl !T.Text !(V.Vector (Pair T.Text (S.Maybe Expression))) !(S.Maybe T.Text) !(V.Vector Statement) !PPosition deriving Eq
data DefineDec     = DefineDec !T.Text !(V.Vector (Pair T.Text (S.Maybe Expression))) !(V.Vector Statement) !PPosition deriving Eq
data Nd            = Nd !NodeDesc !(V.Vector Statement) !(S.Maybe NodeDesc) !PPosition deriving Eq
data VarAss        = VarAss !T.Text !Expression !PPosition deriving Eq
data MFC           = MFC !T.Text !(V.Vector Expression) !PPosition deriving Eq
data SFC           = SFC !HFunctionCall !PPosition deriving Eq -- ^ /Higher order function/ call.
data RColl         = RColl !CollectorType !T.Text !SearchExpression !(V.Vector (Pair T.Text Expression)) !PPosition deriving Eq -- ^ For all types of collectors.
data Dep           = Dep !(Pair T.Text Expression) !(Pair T.Text Expression) !LinkType !PPosition deriving Eq

-- | All the possible statements
data Statement
    = ResourceDeclaration !ResDec
    | DefaultDeclaration !DefaultDec
    | ResourceOverride !ResOver
    | ConditionalStatement !CondStatement
    | ClassDeclaration !ClassDecl
    | DefineDeclaration !DefineDec
    | Node !Nd
    | VariableAssignment !VarAss
    | MainFunctionCall !MFC
    | SHFunctionCall !SFC
    | ResourceCollection !RColl
    | Dependency !Dep
    | TopContainer !(V.Vector Statement) !Statement -- ^ This is a special statement that is used to include the expressions that are top level. This is certainly buggy, but probably just like the original implementation.
    deriving Eq

makeClassy ''HFunctionCall
