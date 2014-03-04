{-# LANGUAGE FlexibleContexts, TemplateHaskell #-}
module Puppet.Lens
 ( -- * Pure resolution prisms
   _PResolveExpression
 , _PResolveValue
 -- * Prisms for PValues
 , _PHash
 , _PBoolean
 , _PString
 , _PResourceReference
 , _PUndef
 , _PArray
 -- * Parsing prism
 , _PParse
 -- * Lenses and Prisms for 'Statement's
 , _ResourceDeclaration
 , _DefaultDeclaration
 , _ResourceOverride
 , _ConditionalStatement
 , _ClassDeclaration
 , _DefineDeclaration
 , _Node
 , _VariableAssignment
 , _MainFunctionCall
 , _SHFunctionCall
 , _ResourceCollection
 , _Dependency
 , _TopContainer
 , _Statements
 ) where

import Control.Lens
import Data.Aeson.Lens
import Control.Applicative

import Puppet.PP (displayNocolor)
import Puppet.Parser.Types
import Puppet.Interpreter.Types
import Puppet.Interpreter.Resolve
import Puppet.Parser
import Puppet.Parser.PrettyPrinter (ppStatements)

import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.Tuple.Strict hiding (uncurry)
import Data.Bits
import Text.Parser.Combinators (eof)

-- Prisms
makePrisms ''PValue
makePrisms ''Statement

-- | Incomplete
_PResolveExpression :: Prism' Expression PValue
_PResolveExpression = prism reinject extract
    where
        extract x@(PValue v) = case v ^? _PResolveValue of
                                   Just r -> Right r
                                   Nothing -> Left x
        extract x@(And a b) =
            let a' = a ^? _PResolveExpression . to pValue2Bool
                b' = b ^? _PResolveExpression . to pValue2Bool
            in  case (a',b') of
                    (Just False, _) -> Right (PBoolean False)
                    (Just _, Just r) -> Right (PBoolean r)
                    _ -> Left x
        extract x@(Or a b) =
            let a' = a ^? _PResolveExpression . to pValue2Bool
                b' = b ^? _PResolveExpression . to pValue2Bool
            in  case (a',b') of
                    (Just True, _) -> Right (PBoolean True)
                    (Just _, Just r) -> Right (PBoolean r)
                    _ -> Left x
        extract x@(Addition a b)       = extractBinop x a b (+) (+)
        extract x@(Substraction a b)   = extractBinop x a b (-) (-)
        extract x@(Division a b)       = extractNotZero b >> extractBinop x a b div (/)
        extract x@(Multiplication a b) = extractBinop x a b (*) (*)
        extract x@(Modulo a b)         = extractNotZero b >> extractIntOp x a b mod
        extract x@(RightShift a b)     = extractIntOp x a b (\v -> shiftR v . fromIntegral)
        extract x@(LeftShift a b)      = extractIntOp x a b (\v -> shiftL v . fromIntegral)
        extract x                      = Left x
        reinject                       = PValue . review _PResolveValue

extractNotZero :: Expression -> Either Expression PValue
extractNotZero e = case e ^? _PResolveExpression of
                       Just "0" -> Left e
                       Just r   -> Right r
                       _        -> Left e

extractBinop :: Expression -> Expression -> Expression -> (Integer -> Integer -> Integer) -> (Double -> Double -> Double) -> Either Expression PValue
extractBinop x a b opi opf = case opi `fmap` (a ^? _PResolveExpression . _Integer) <*> (b ^? _PResolveExpression . _Integer) of
                                 Just ri -> Right $ review _Integer ri
                                 Nothing -> case opf `fmap` (a ^? _PResolveExpression . _Double) <*> (b ^? _PResolveExpression . _Double) of
                                                Just rd -> Right $ review _Double rd
                                                Nothing -> Left x

extractIntOp :: Expression -> Expression -> Expression -> (Integer -> Integer -> Integer) -> Either Expression PValue
extractIntOp x a b opi = case opi `fmap` (a ^? _PResolveExpression . _Integer) <*> (b ^? _PResolveExpression . _Integer) of
                             Just ri -> Right $ review _Integer ri
                             Nothing -> Left x

_PResolveValue :: Prism' UValue PValue
_PResolveValue = prism toU toP
    where
        toP (UString s) = Right (PString s)
        toP UUndef = Right PUndef
        toP (UBoolean b) = Right (PBoolean b)
        toP r@(UResourceReference t n) = maybe (Left r) (Right . PResourceReference t) (n ^? _PResolveExpression . _PString)
        toP r@(UArray lst) = maybe (Left r) (Right . PArray) (V.mapM (preview _PResolveExpression) lst)
        toP r@(UHash lst) = maybe (Left r) (Right . PHash . HM.fromList) (mapM resolveKV (V.toList lst))
            where
                resolveKV (k :!: v) = do
                    k' <- k ^? _PResolveExpression . _PString
                    v' <- v ^? _PResolveExpression
                    return (k',v')
        toP r@(UInterpolable ip) = maybe (Left r) (Right . PString . T.concat . V.toList ) (V.mapM (preview (_PResolveValue . _PString)) ip)
        toP r = Left r
        toU (PBoolean x) = UBoolean x
        toU PUndef = UUndef
        toU (PString s) = UString s
        toU (PResourceReference t n) = UResourceReference t (PValue (UString n))
        toU (PArray r) = UArray (fmap (PValue . toU) r)
        toU (PHash h) = UHash (V.fromList $ map (\(k,v) -> (PValue (UString k) :!: PValue (toU v))) $ HM.toList h)

_PParse :: Prism' T.Text (V.Vector Statement)
_PParse = prism dspl prs
    where
        prs i = case runMyParser (puppetParser <* eof) "dummy" i of
                Left _  -> Left i
                Right x -> Right x
        dspl = T.pack . displayNocolor . ppStatements

-- | Extracts the statements from 'ClassDeclaration', 'DefineDeclaration',
-- 'Node' and the spurious statements of 'TopContainer'.
_Statements :: Lens' Statement [Statement]
_Statements = lens (V.toList . sget) (\s v -> sset s (V.fromList v))
    where
        sget :: Statement -> V.Vector Statement
        sget (ClassDeclaration _ _ _ s _) = s
        sget (DefineDeclaration _ _ s _) = s
        sget (Node _ s _ _) = s
        sget (TopContainer s _) = s
        sget (SHFunctionCall (HFunctionCall _ _ _ s _) _) = s
        sget _ = V.empty
        sset :: Statement -> V.Vector Statement -> Statement
        sset (ClassDeclaration n args inh _ p) s = ClassDeclaration n args inh s p
        sset (Node ns _ nd' p) s = Node ns s nd' p
        sset (DefineDeclaration n args _ p) s = DefineDeclaration n args s p
        sset (TopContainer _ p) s = TopContainer s p
        sset (SHFunctionCall (HFunctionCall t e pr _ e2) p) s = SHFunctionCall (HFunctionCall t e pr s e2) p
        sset x _ = x
