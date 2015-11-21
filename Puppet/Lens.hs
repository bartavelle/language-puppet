{-# LANGUAGE FlexibleContexts, TemplateHaskell #-}
module Puppet.Lens
 ( -- * Pure resolution prisms
   _PResolveExpression
 , _PResolveValue
 -- * Prisms for PValues
 , _PHash
 , _PBoolean
 , _PString
 , _PNumber
 , _PResourceReference
 , _PUndef
 , _PArray
 -- * Parsing prism
 -- * Lenses and Prisms for 'Statement's
 , _ConditionalStatement
 , _ClassDeclaration
 , _DefineDeclaration
 , _Node
 , _VariableAssignment
 , _MainFunctionCall
 , _SHFunctionCall
 , _Dependency
 , _TopContainer
 , _Statements
 -- * More primitive prisms for 'Statement's
 , _ResourceDeclaration'
 , _DefaultDeclaration'
 , _ResourceOverride'
 , _ConditionalStatement'
 , _ClassDeclaration'
 , _DefineDeclaration'
 , _Node'
 , _VariableAssignment'
 , _MainFunctionCall'
 , _SHFunctionCall'
 , _Dependency'
 -- * Lenses and Prisms for 'Expression's
 , _Equal
 , _Different
 , _Not
 , _And
 , _Or
 , _LessThan
 , _MoreThan
 , _LessEqualThan
 , _MoreEqualThan
 , _RegexMatch
 , _NotRegexMatch
 , _Contains
 , _Addition
 , _Substraction
 , _Division
 , _Multiplication
 , _Modulo
 , _RightShift
 , _LeftShift
 , _Lookup
 , _Negate
 , _ConditionalValue
 , _FunctionApplication
 , _Terminal
 -- * Prisms for exceptions
 , _PrettyError
 ) where

import Control.Lens

import Puppet.Parser.Types
import Puppet.Interpreter.Types
import Puppet.Interpreter.Pure
import Puppet.Interpreter.Resolve

import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Maybe.Strict as S
import Data.Tuple.Strict hiding (uncurry)
import Control.Exception (SomeException, toException, fromException)

-- Prisms
makePrisms ''PValue
--makePrisms ''Statement
makePrisms ''Expression

makePrisms ''ResDec
makePrisms ''DefaultDec
makePrisms ''ResOver
makePrisms ''CondStatement
makePrisms ''ClassDecl
makePrisms ''DefineDec
makePrisms ''Nd
makePrisms ''VarAss
makePrisms ''MFC
makePrisms ''SFC
makePrisms ''RColl
makePrisms ''Dep


_PrettyError :: Prism' SomeException PrettyError
_PrettyError = prism' toException fromException

_ResourceDeclaration' :: Prism' Statement ResDec
_ResourceDeclaration' = prism ResourceDeclaration $ \x -> case x of
                                                              ResourceDeclaration a -> Right a
                                                              _ -> Left x
_DefaultDeclaration' :: Prism' Statement DefaultDec
_DefaultDeclaration' = prism DefaultDeclaration $ \x -> case x of
                                                            DefaultDeclaration a -> Right a
                                                            _ -> Left x
_ResourceOverride' :: Prism' Statement ResOver
_ResourceOverride' = prism ResourceOverride $ \x -> case x of
                                                        ResourceOverride a -> Right a
                                                        _ -> Left x
_ConditionalStatement' :: Prism' Statement CondStatement
_ConditionalStatement' = prism ConditionalStatement $ \x -> case x of
                                                                ConditionalStatement a -> Right a
                                                                _ -> Left x
_ClassDeclaration' :: Prism' Statement ClassDecl
_ClassDeclaration' = prism ClassDeclaration $ \x -> case x of
                                                        ClassDeclaration a -> Right a
                                                        _ -> Left x
_DefineDeclaration' :: Prism' Statement DefineDec
_DefineDeclaration' = prism DefineDeclaration $ \x -> case x of
                                                          DefineDeclaration a -> Right a
                                                          _ -> Left x
_Node' :: Prism' Statement Nd
_Node' = prism Node $ \x -> case x of
                                Node a -> Right a
                                _      -> Left x

_VariableAssignment' :: Prism' Statement VarAss
_VariableAssignment' = prism VariableAssignment $ \x -> case x of
                                                            VariableAssignment a -> Right a
                                                            _ -> Left x
_MainFunctionCall' :: Prism' Statement MFC
_MainFunctionCall' = prism MainFunctionCall $ \x -> case x of
                                                        MainFunctionCall a -> Right a
                                                        _ -> Left x
_SHFunctionCall' :: Prism' Statement SFC
_SHFunctionCall' = prism SHFunctionCall $ \x -> case x of
                                                    SHFunctionCall a -> Right a
                                                    _ -> Left x
_Dependency' :: Prism' Statement Dep
_Dependency' = prism Dependency $ \x -> case x of
                                            Dependency a -> Right a
                                            _ -> Left x
_TopContainer :: Prism' Statement (V.Vector Statement, Statement)
_TopContainer = prism (uncurry TopContainer) $ \x -> case x of
                                                         TopContainer vs s -> Right (vs,s)
                                                         _ -> Left x

_ConditionalStatement :: Prism' Statement (V.Vector (Pair Expression (V.Vector Statement)), PPosition)
_ConditionalStatement = _ConditionalStatement' . _CondStatement
_ClassDeclaration :: Prism' Statement (T.Text, V.Vector (Pair T.Text (S.Maybe Expression)), S.Maybe T.Text, V.Vector Statement, PPosition)
_ClassDeclaration = _ClassDeclaration' . _ClassDecl
_DefineDeclaration :: Prism' Statement (T.Text, V.Vector (Pair T.Text (S.Maybe Expression)), V.Vector Statement, PPosition)
_DefineDeclaration = _DefineDeclaration' . _DefineDec
_Node :: Prism' Statement (NodeDesc, V.Vector Statement, S.Maybe NodeDesc, PPosition)
_Node = _Node' . _Nd
_VariableAssignment :: Prism' Statement (T.Text, Expression, PPosition)
_VariableAssignment = _VariableAssignment' . _VarAss
_MainFunctionCall :: Prism' Statement (T.Text, V.Vector Expression, PPosition)
_MainFunctionCall = _MainFunctionCall' . _MFC
_SHFunctionCall :: Prism' Statement (HFunctionCall, PPosition)
_SHFunctionCall = _SHFunctionCall' . _SFC
_Dependency :: Prism' Statement (Pair T.Text Expression, Pair T.Text Expression, LinkType, PPosition)
_Dependency = _Dependency' . _Dep

-- | Incomplete
_PResolveExpression :: Prism' Expression PValue
_PResolveExpression = prism reinject extract
    where
        extract e = case dummyEval (resolveExpression e) of
                        Right x -> Right x
                        Left _  -> Left e
        reinject  = Terminal . review _PResolveValue

_PResolveValue :: Prism' UValue PValue
_PResolveValue = prism toU toP
    where
        toP uv = case dummyEval (resolveValue uv) of
                     Right x -> Right x
                     Left _  -> Left uv
        toU (PBoolean x) = UBoolean x
        toU (PNumber x) = UNumber x
        toU PUndef = UUndef
        toU (PString s) = UString s
        toU (PResourceReference t n) = UResourceReference t (Terminal (UString n))
        toU (PArray r) = UArray (fmap (Terminal . toU) r)
        toU (PHash h) = UHash (V.fromList $ map (\(k,v) -> (Terminal (UString k) :!: Terminal (toU v))) $ HM.toList h)

-- | Extracts the statements from 'ClassDeclaration', 'DefineDeclaration',
-- 'Node' and the spurious statements of 'TopContainer'.
_Statements :: Lens' Statement [Statement]
_Statements = lens (V.toList . sget) (\s v -> sset s (V.fromList v))
    where
        sget :: Statement -> V.Vector Statement
        sget (ClassDeclaration (ClassDecl _ _ _ s _)) = s
        sget (DefineDeclaration (DefineDec _ _ s _)) = s
        sget (Node (Nd _ s _ _)) = s
        sget (TopContainer s _) = s
        sget (SHFunctionCall (SFC (HFunctionCall _ _ _ s _) _)) = s
        sget _ = V.empty
        sset :: Statement -> V.Vector Statement -> Statement
        sset (ClassDeclaration (ClassDecl n args inh _ p)) s = ClassDeclaration (ClassDecl n args inh s p)
        sset (Node (Nd ns _ nd' p)) s = Node (Nd ns s nd' p)
        sset (DefineDeclaration (DefineDec n args _ p)) s = DefineDeclaration (DefineDec n args s p)
        sset (TopContainer _ p) s = TopContainer s p
        sset (SHFunctionCall (SFC (HFunctionCall t e pr _ e2) p)) s = SHFunctionCall (SFC (HFunctionCall t e pr s e2) p)
        sset x _ = x
