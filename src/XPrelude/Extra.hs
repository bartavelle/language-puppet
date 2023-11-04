{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_HADDOCK ignore-exports #-}

module XPrelude.Extra
  ( module Exports,
    String,
    Container,
    unwrapError,
    dropInitialColons,
    strictifyEither,
    scientific2text,
    text2Scientific,
    ifromList,
    ikeys,
    ifromListWith,
    iunionWith,
    iinsertWith,

    -- * Logger
    loggerName,
    logDebug,
    logDebugStr,
    logInfo,
    logInfoStr,
    logWarning,
    logWarningStr,
    logError,
    logErrorStr,
    logCritical,
    logCriticalStr,

    -- * Lenses for json
    avalues,
    nth,
  )
where

import Control.Exception.Lens as Exports (catching)
import Control.Lens as Exports hiding
  ( Strict,
    argument,
    noneOf,
    op,
    (<.>),
  )
import Control.Monad as Exports (fail)
import Control.Monad.Trans.Except as Exports (except)
import Control.Monad.Trans.Maybe as Exports (runMaybeT)
import Data.Aeson (Value (..))
import Data.Aeson as Exports (FromJSON, ToJSON, fromJSON, toJSON)
import Data.Aeson.Lens as Exports (key, _Array, _Object, _String)
import Data.Attoparsec.Text (parseOnly, rational)
import qualified Data.Either.Strict as S
import Data.HashMap.Strict as Exports (HashMap)
import qualified Data.HashMap.Strict as Map
import Data.HashSet as Exports (HashSet)
import qualified Data.HashSet as HS
import Data.Scientific as Exports (Scientific)
import qualified Data.Scientific as Scientific
import Data.String (String)
import Data.String as Exports (IsString (..))
import qualified Data.Text as Text
import Data.Tuple.Strict as Exports (Pair (..))
import Data.Vector as Exports (Vector)
import Protolude as Exports hiding
  ( Down,
    Infix,
    Prefix,
    Selector,
    State,
    StateT,
    break,
    check,
    evalState,
    evalStateT,
    execState,
    execStateT,
    from,
    hash,
    list,
    moduleName,
    runState,
    runStateT,
    sourceColumn,
    sourceLine,
    to,
    typeOf,
    uncons,
    unsnoc,
    withState,
    (%),
    (<&>),
    (<.>),
  )
import qualified System.Log.Logger as Log
import Text.Regex.PCRE.ByteString.Utils as Exports (Regex)
import XPrelude.PP

type Container = Map.HashMap Text

text2Scientific :: Text -> Maybe Scientific
text2Scientific t = rightToMaybe (parseOnly rational t)

scientific2text :: Scientific -> Text
scientific2text n =
  case Scientific.floatingOrInteger n of
    Left r -> show (r :: Double)
    Right i -> show (i :: Integer)

strictifyEither :: Either a b -> S.Either a b
strictifyEither (Left x) = S.Left x
strictifyEither (Right x) = S.Right x

-- | Helper for hashmap, in case we want another kind of map.
ifromList :: (Monoid m, At m, Foldable f) => f (Index m, IxValue m) -> m
{-# INLINEABLE ifromList #-}
ifromList = foldl' (\curm (k, v) -> curm & at k ?~ v) mempty

-- | Return all the keys of a map in a set.
ikeys :: (Eq k, Hashable k) => HashMap k v -> HS.HashSet k
{-# INLINEABLE ikeys #-}
ikeys = HS.fromList . Map.keys

ifromListWith :: (Monoid m, At m, Foldable f) => (IxValue m -> IxValue m -> IxValue m) -> f (Index m, IxValue m) -> m
{-# INLINEABLE ifromListWith #-}
ifromListWith f = foldl' (\curmap (k, v) -> iinsertWith f k v curmap) mempty

iinsertWith :: (At m) => (IxValue m -> IxValue m -> IxValue m) -> Index m -> IxValue m -> m -> m
{-# INLINEABLE iinsertWith #-}
iinsertWith f k v m =
  m & at k %~ mightreplace
  where
    mightreplace Nothing = Just v
    mightreplace (Just x) = Just (f v x)

iunionWith :: (Hashable k, Eq k) => (v -> v -> v) -> HashMap k v -> HashMap k v -> HashMap k v
{-# INLINEABLE iunionWith #-}
iunionWith = Map.unionWith

-- | Remove the '::' token from a text if any.
dropInitialColons :: Text -> Text
dropInitialColons t = fromMaybe t (Text.stripPrefix "::" t)

loggerName :: String
loggerName = "language-puppet"

logDebug :: Text -> IO ()
logDebug = Log.debugM "language-puppet" . toS

logInfo :: Text -> IO ()
logInfo = Log.infoM "language-puppet" . toS

logInfoStr :: String -> IO ()
logInfoStr = Log.infoM "language-puppet"

logWarning :: Text -> IO ()
logWarning = Log.warningM "language-puppet" . toS

logWarningStr :: String -> IO ()
logWarningStr = Log.warningM "language-puppet"

logError :: Text -> IO ()
logError = Log.errorM "language-puppet" . toS

logErrorStr :: String -> IO ()
logErrorStr = Log.errorM "language-puppet"

logCritical :: Text -> IO ()
logCritical = Log.criticalM "language-puppet" . toS

logCriticalStr :: String -> IO ()
logCriticalStr = Log.criticalM "language-puppet"

logDebugStr :: String -> IO ()
logDebugStr = Log.debugM "language-puppet"

-- | In case of a Left error, print and exit immediately.
unwrapError :: Doc -> Either PrettyError a -> IO a
unwrapError desc = either exit pure
  where
    exit err = putDoc (display err) >> exitFailure
    display err = red desc <> ":" <+> getError err

avalues :: IndexedTraversal' Int Value Value
avalues = _Array . traversed

nth :: Int -> Traversal' Value Value
nth i = _Array . ix i
