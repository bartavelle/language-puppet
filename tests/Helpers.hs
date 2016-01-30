{-# LANGUAGE OverloadedLists #-}
module Helpers ( compileCatalog
               , getCatalog
               , getResource
               , getAttribute
               , spretty
               ) where

import           Puppet.Interpreter (computeCatalog)
import           Puppet.Interpreter.Pure
import           Puppet.Interpreter.Types
import           Puppet.Parser
import           Puppet.Parser.Types
import           Puppet.PP

import           Control.Lens
import qualified Data.HashMap.Strict as HM
import qualified Data.Maybe.Strict as S
import           Data.Text (Text, unpack)

compileCatalog :: Monad m => Text -> m (FinalCatalog, EdgeMap, FinalCatalog, [Resource], InterpreterState)
compileCatalog input = do
    statements <- either (fail . show) return (runPParser "dummy" input)
    let nodename = "node.fqdn"
        sttmap = [( (TopNode, nodename), NodeDeclaration (NodeDecl (NodeName nodename) statements S.Nothing (initialPPos "dummy")) ) ]
        (res, finalState, _) = pureEval dummyFacts sttmap (computeCatalog nodename)
    (catalog,em,exported,defResources) <- either (fail . show) return res
    return (catalog,em,exported,defResources,finalState)

getCatalog :: Monad m => Text -> m FinalCatalog
getCatalog = fmap (view _1) . compileCatalog

spretty :: Pretty a => a -> String
spretty = flip displayS "" . renderCompact . pretty

getResource :: Monad m => RIdentifier -> FinalCatalog -> m Resource
getResource resid catalog = maybe (fail ("Unknown resource " ++ spretty resid)) return (HM.lookup resid catalog)

getAttribute :: Monad m => Text -> Resource -> m PValue
getAttribute att res = case res ^? rattributes . ix att of
                           Nothing -> fail ("Unknown attribute: " ++ unpack att)
                           Just x -> return x

