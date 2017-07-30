{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleContexts #-}
module Helpers ( compileCatalog
               , getCatalog
               , getResource
               , getAttribute
               , spretty
               , withStdlibFunction
               ) where

import           Puppet.Interpreter (computeCatalog)
import           Puppet.Interpreter.Pure
import           Puppet.Interpreter.Types
import           Puppet.Parser
import           Puppet.Parser.Types
import           Puppet.PP
import           Puppet.Stdlib

import           Control.Lens
import qualified Data.HashMap.Strict as HM
import qualified Data.Maybe.Strict as S
import           Data.Text (Text, unpack)
import           Test.Hspec

compileCatalog :: MonadError String m => Text -> m (FinalCatalog, EdgeMap, FinalCatalog, [Resource], InterpreterState)
compileCatalog input = do
    statements <- either (throwError . show) return (runPParser "dummy" input)
    let nodename = "node.fqdn"
        sttmap = [( (TopNode, nodename), NodeDeclaration (NodeDecl (NodeName nodename) statements S.Nothing (initialPPos "dummy")) ) ]
        (res, finalState, _) = pureEval dummyFacts sttmap (computeCatalog nodename)
    (catalog,em,exported,defResources) <- either (throwError . show) return res
    return (catalog,em,exported,defResources,finalState)

getCatalog :: MonadError String m => Text -> m FinalCatalog
getCatalog = fmap (view _1) . compileCatalog

spretty :: Pretty a => a -> String
spretty = flip displayS "" . renderCompact . pretty

getResource :: Monad m => RIdentifier -> FinalCatalog -> m Resource
getResource resid catalog = maybe (fail ("Unknown resource " ++ spretty resid)) return (HM.lookup resid catalog)

getAttribute :: Monad m => Text -> Resource -> m PValue
getAttribute att res = case res ^? rattributes . ix att of
                           Nothing -> fail ("Unknown attribute: " ++ unpack att)
                           Just x -> return x

withStdlibFunction :: Text -> ( ([PValue] -> InterpreterMonad PValue) -> Spec ) -> Spec
withStdlibFunction fname testsuite =
    case stdlibFunctions ^? ix fname of
        Just f -> testsuite f
        Nothing -> fail ("Don't know this function: " ++ show fname)

