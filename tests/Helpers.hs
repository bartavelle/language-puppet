{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}

module Helpers ( module Exports
               , checkExprsSuccess
               , checkExprsError
               , pureCatalog
               , getResource
               , getAttribute
               , renderToString
               , withStdlibFunction
               ) where

import           XPrelude            as Exports

import           Control.Monad.Fail  as Exports (MonadFail)
import qualified Data.HashMap.Strict as HM
import qualified Data.Maybe.Strict   as S
import qualified Data.Vector         as Vector
import           Test.Hspec          as Exports

import           Puppet.Interpreter  as Exports
import           Puppet.Parser       as Exports
import           Puppet.Runner       as Exports hiding (getCatalog)

-- | Given a raw text input to be parsed, compute the manifest in a pure setting.
-- The 'InterpreterWriter' might be useful for debugging purpose.
pureCatalog ::  Text -> Either String (FinalCatalog, InterpreterWriter)
pureCatalog = runExcept . fmap (\s -> (s^._1,s^._6)) . compileCatalog
  where
  compileCatalog :: Text -> Except String (FinalCatalog, EdgeMap, FinalCatalog, [Resource], InterpreterState, InterpreterWriter)
  compileCatalog input = do
    statements <- either (throwError . show) pure (runPuppetParser mempty input)
    let nodename = "pure"
        top_node = [((TopNode, nodename), NodeDeclaration (NodeDecl (NodeName nodename) statements S.Nothing (initialPPos mempty)))]
        (res, finalState, logs) = pureEval top_node (computeCatalog nodename)
    (catalog, em, exported, defResources) <- either (throwError . show) pure res
    pure (catalog, em, exported, defResources, finalState, logs)

getResource :: (MonadFail m) => RIdentifier -> FinalCatalog -> m Resource
getResource resid catalog = maybe (fail ("Unknown resource " <> renderToString resid)) pure (HM.lookup resid catalog)

getAttribute :: MonadFail m => Text -> Resource -> m PValue
getAttribute att res =
  case res ^? rattributes . ix att of
    Nothing -> fail ("Unknown attribute: " <> toS att)
    Just x  -> return x

withStdlibFunction :: Text -> ( ([PValue] -> InterpreterMonad PValue) -> Spec ) -> Spec
withStdlibFunction fname testsuite =
  case stdlibFunctions ^? ix fname of
    Just f  -> testsuite f
    Nothing -> panic ("Don't know this function: " <> fname)

checkExprsSuccess :: Text ->  [Expression] -> Text -> Expectation
checkExprsSuccess fname args res =
  case evalExprs fname args of
    Left rr    -> expectationFailure (show rr)
    Right res' -> res' `shouldBe` res

checkExprsError :: Text ->  [Expression] -> String -> Expectation
checkExprsError fname args msg =
  case evalExprs fname args of
    Left rr -> show rr `shouldContain` msg
    Right r -> expectationFailure ("Should have errored, received this instead: " <> show r)

evalExprs :: Text -> [Expression] -> Either PrettyError Text
evalExprs fname =
  dummyEval . resolveValue . UFunctionCall fname . Vector.fromList >=> \case
    PString s -> return s
    v         -> Left ("Expected a string, not " <> PrettyError (pretty v))

renderToString :: Pretty a => a -> String
renderToString d = displayS (renderCompact (pretty d)) ""
