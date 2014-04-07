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
import Control.Applicative

import Puppet.PP (displayNocolor)
import Puppet.Parser.Types
import Puppet.Interpreter.Types
import Puppet.Interpreter.Pure
import Puppet.Interpreter.Resolve
import Puppet.Parser
import Puppet.Parser.PrettyPrinter (ppStatements)

import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.Tuple.Strict hiding (uncurry)
import Text.Parser.Combinators (eof)

-- Prisms
makePrisms ''PValue
makePrisms ''Statement

-- | Incomplete
_PResolveExpression :: Prism' Expression PValue
_PResolveExpression = prism reinject extract
    where
        extract e = case dummyEval (resolveExpression e) of
                        Right x -> Right x
                        Left _  -> Left e
        reinject  = PValue . review _PResolveValue

_PResolveValue :: Prism' UValue PValue
_PResolveValue = prism toU toP
    where
        toP uv = case dummyEval (resolveValue uv) of
                     Right x -> Right x
                     Left _  -> Left uv
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
