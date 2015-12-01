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
 , _Statements
 , _ResDecl
 , _ResDefaultDecl
 , _ResOverrDecl
 , _ResCollDecl
 , _ConditionalDecl
 , _ClassDecl
 , _DefineDecl
 , _NodeDecl
 , _VarAssignDecl
 , _MainFuncDecl
 , _HigherOrderLambdaDecl
 , _DepDecl
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
import Data.Tuple.Strict hiding (uncurry)
import Control.Exception (SomeException, toException, fromException)

-- Prisms
makePrisms ''PValue
--makePrisms ''Statement
makePrisms ''Expression

_PrettyError :: Prism' SomeException PrettyError
_PrettyError = prism' toException fromException

_ResDecl :: Prism' Statement ResDecl
_ResDecl = prism ResourceDeclaration $ \x -> case x of
                                                 ResourceDeclaration a -> Right a
                                                 _ -> Left x
_ResDefaultDecl :: Prism' Statement ResDefaultDecl
_ResDefaultDecl = prism ResourceDefaultDeclaration $ \x -> case x of
                                                               ResourceDefaultDeclaration a -> Right a
                                                               _ -> Left x
_ResOverrDecl :: Prism' Statement ResOverrideDecl
_ResOverrDecl = prism ResourceOverrideDeclaration $ \x -> case x of
                                                              ResourceOverrideDeclaration a -> Right a
                                                              _ -> Left x
_ResCollDecl :: Prism' Statement ResCollDecl
_ResCollDecl = prism ResourceCollectionDeclaration $ \x -> case x of
                                                               ResourceCollectionDeclaration a -> Right a
                                                               _ -> Left x
_ConditionalDecl :: Prism' Statement ConditionalDecl
_ConditionalDecl = prism ConditionalDeclaration $ \x -> case x of
                                                            ConditionalDeclaration a -> Right a
                                                            _ -> Left x
_ClassDecl :: Prism' Statement ClassDecl
_ClassDecl = prism ClassDeclaration $ \x -> case x of
                                                ClassDeclaration a -> Right a
                                                _ -> Left x
_DefineDecl :: Prism' Statement DefineDecl
_DefineDecl = prism DefineDeclaration $ \x -> case x of
                                                  DefineDeclaration a -> Right a
                                                  _ -> Left x
_NodeDecl :: Prism' Statement NodeDecl
_NodeDecl = prism NodeDeclaration $ \x -> case x of
                                              NodeDeclaration a -> Right a
                                              _      -> Left x

_VarAssignDecl :: Prism' Statement VarAssignDecl
_VarAssignDecl = prism VarAssignmentDeclaration $ \x -> case x of
                                                            VarAssignmentDeclaration a -> Right a
                                                            _ -> Left x
_MainFuncDecl :: Prism' Statement MainFuncDecl
_MainFuncDecl = prism MainFunctionDeclaration $ \x -> case x of
                                                          MainFunctionDeclaration a -> Right a
                                                          _ -> Left x
_HigherOrderLambdaDecl :: Prism' Statement HigherOrderLambdaDecl
_HigherOrderLambdaDecl = prism HigherOrderLambdaDeclaration $ \x -> case x of
                                                                        HigherOrderLambdaDeclaration a -> Right a
                                                                        _ -> Left x
_DepDecl :: Prism' Statement DepDecl
_DepDecl = prism DependencyDeclaration $ \x -> case x of
                                                   DependencyDeclaration a -> Right a
                                                   _ -> Left x

_TopContainer :: Prism' Statement (V.Vector Statement, Statement)
_TopContainer = prism (uncurry TopContainer) $ \x -> case x of
                                                         TopContainer vs s -> Right (vs,s)
                                                         _ -> Left x

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
        sget (DefineDeclaration (DefineDecl _ _ s _)) = s
        sget (NodeDeclaration (NodeDecl _ s _ _)) = s
        sget (TopContainer s _) = s
        sget (HigherOrderLambdaDeclaration (HigherOrderLambdaDecl (HOLambdaCall _ _ _ s _) _)) = s
        sget _ = V.empty
        sset :: Statement -> V.Vector Statement -> Statement
        sset (ClassDeclaration (ClassDecl n args inh _ p)) s = ClassDeclaration (ClassDecl n args inh s p)
        sset (NodeDeclaration (NodeDecl ns _ nd' p)) s = NodeDeclaration (NodeDecl ns s nd' p)
        sset (DefineDeclaration (DefineDecl n args _ p)) s = DefineDeclaration (DefineDecl n args s p)
        sset (TopContainer _ p) s = TopContainer s p
        sset (HigherOrderLambdaDeclaration (HigherOrderLambdaDecl (HOLambdaCall t e pr _ e2) p)) s = HigherOrderLambdaDeclaration (HigherOrderLambdaDecl (HOLambdaCall t e pr s e2) p)
        sset x _ = x
