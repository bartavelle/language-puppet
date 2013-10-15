{-# LANGUAGE DeriveGeneric, TemplateHaskell #-}
module Puppet.Parser.Types where

import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Tuple.Strict
import qualified Data.Maybe.Strict as S
import GHC.Generics
import Data.Char (toUpper)
import Text.Regex.PCRE.String
import Control.Lens

import Text.Parsec.Pos

-- | Properly capitalizes resource types
capitalizeRT :: T.Text -> T.Text
capitalizeRT = T.intercalate "::" . map capitalize' . T.splitOn "::"

capitalize' :: T.Text -> T.Text
capitalize' t | T.null t = T.empty
              | otherwise = T.cons (toUpper (T.head t)) (T.tail t)

type PPosition = Pair Position Position

type Position = SourcePos

initialPPos :: T.Text -> PPosition
initialPPos x =
    let i = initialPos (T.unpack x)
    in (i :!: i)

data HigherFuncType = HFEach
                    | HFMap
                    | HFReduce
                    | HFFilter
                    | HFSlice
                    deriving Eq

data BlockParameters = BPSingle !T.Text
                     | BPPair   !T.Text !T.Text
                     deriving Eq

-- used for the each/filter/etc. "higher level functions"
data HFunctionCall = HFunctionCall { _hftype       :: !HigherFuncType
                                   , _hfexpr       :: !(S.Maybe Expression)
                                   , _hfparams     :: !BlockParameters
                                   , _hfstatements :: !(V.Vector Statement)
                                   , _hfexpression :: !(S.Maybe Expression)
                                   }
                   deriving Eq

data UValue
    = UBoolean !Bool
    | UString !T.Text
    | UInterpolable !(V.Vector UValue)
    | UUndef
    | UResourceReference !T.Text !Expression
    | UArray !(V.Vector Expression)
    | UHash !(V.Vector (Pair Expression Expression))
    | URegexp !T.Text !Regex
    | UVariableReference !T.Text
    | UFunctionCall !T.Text !(V.Vector Expression)
    | UHFunctionCall !HFunctionCall

-- manual instance because of the Regex problem
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
    (==) _ _ = False

array :: [Expression] -> UValue
array = UArray . V.fromList

data SelectorCase = SelectorValue UValue
                  | SelectorDefault
                  deriving (Eq)

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
    | ConditionalValue !Expression !(V.Vector (Pair SelectorCase Expression))
    | FunctionApplication !Expression !Expression
    | PValue !UValue
    deriving (Eq)

data SearchExpression
    = EqualitySearch !T.Text !Expression
    | NonEqualitySearch !T.Text !Expression
    | AndSearch !SearchExpression !SearchExpression
    | OrSearch !SearchExpression !SearchExpression
    | AlwaysTrue
    deriving Eq

data CollectorType = Collector | ExportedCollector
    deriving (Eq)

toBool :: UValue -> Bool
toBool (UString "")      = False
toBool (UInterpolable v) = not (V.null v)
toBool UUndef            = False
toBool (UBoolean x)      = x
toBool _                 = True

data Virtuality = Normal | Virtual | Exported | ExportedRealized
    deriving (Generic, Eq)

data NodeDesc = NodeName !T.Text
              | NodeMatch !T.Text !Regex
              | NodeDefault

instance Eq NodeDesc where
    (==) (NodeName a) (NodeName b) = a == b
    (==) NodeDefault NodeDefault = True
    (==) (NodeMatch a _) (NodeMatch b _) = a == b
    (==) _ _ = False

data Statement
    = ResourceDeclaration !T.Text !Expression !(V.Vector (Pair T.Text Expression)) !Virtuality !PPosition
    | DefaultDeclaration !T.Text !(V.Vector (Pair T.Text Expression)) !PPosition
    | ResourceOverride !T.Text !Expression !(V.Vector (Pair T.Text Expression)) !PPosition
    | ConditionalStatement !(V.Vector (Pair Expression (V.Vector Statement))) !PPosition
    | ClassDeclaration !T.Text !(V.Vector (Pair T.Text (S.Maybe Expression))) !(S.Maybe T.Text) !(V.Vector Statement) !PPosition
    | DefineDeclaration !T.Text !(V.Vector (Pair T.Text (S.Maybe Expression))) !(V.Vector Statement) !PPosition
    | Node !NodeDesc !(V.Vector Statement) !(S.Maybe NodeDesc) !PPosition
    | VariableAssignment !T.Text !Expression !PPosition
    | MainFunctionCall !T.Text !(V.Vector Expression) !PPosition
    | SHFunctionCall !HFunctionCall !PPosition
    | ResourceCollection !CollectorType !T.Text !SearchExpression !(V.Vector (Pair T.Text Expression)) !PPosition
    | Dependency !(Pair T.Text Expression) !(Pair T.Text Expression) !PPosition
    | TopContainer !(V.Vector Statement) !Statement
    deriving Eq

makeClassy ''HFunctionCall

