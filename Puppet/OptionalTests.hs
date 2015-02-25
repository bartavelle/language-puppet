{-# LANGUAGE LambdaCase      #-}
-- | The module works in IO and exits on failure.
-- It is meant to be use by a `Daemon`.
-- PS: if more flexibility is needed, we can `throwM`
-- on failure and let the caller choose what to do.
module Puppet.OptionalTests (testCatalog) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad                    (unless)
import           Control.Monad.Trans              (liftIO)
import           Control.Monad.Trans.Except
import           Data.Foldable                    (asum, toList, elem, traverse_)
import           Data.Maybe                       (mapMaybe)
import           Data.Monoid                      ((<>))
import qualified Data.Text                        as T
import           Prelude                          hiding (all, elem)
import           Puppet.Interpreter.PrettyPrinter ()
import           Puppet.Interpreter.Types
import           Puppet.PP                        hiding ((<$>))
import           System.Exit                      (exitFailure)
import           System.Posix.Files

-- | Entry point for all optional tests
testCatalog :: FilePath -> FinalCatalog -> IO ()
testCatalog = testFileSources

-- | Test source for every file resources in the catalog.
testFileSources :: FilePath -> FinalCatalog -> IO ()
testFileSources basedir c = do
    let getFiles = filter presentFile . toList
        presentFile r = r ^. rid . itype == "file"
                        && (r ^. rattributes . at "ensure") `elem` [Nothing, Just "present"]
                        && r ^. rattributes . at "source" /= Just PUndef
        getSource = mapMaybe (\r -> (,) <$> pure r <*> r ^. rattributes . at "source")
    checkAllSources basedir $ (getSource . getFiles) c

-- | Check source for all file resources and append failures along.
checkAllSources :: FilePath -> [(Resource, PValue)] -> IO ()
checkAllSources fp fs = go fs []
  where
    go ((res, filesource):xs) es =
      runExceptT (checkFile fp filesource) >>= \case
        Right () -> go xs es
        Left err -> go xs ((PrettyError $ "Could not find " <+> pretty filesource <> semi
                           <+> align (vsep [getError err, showPos (res^.rpos^._1)])):es)
    go [] [] = pure ()
    go [] es = do
      traverse_ (\e -> putDoc $ getError e <> line) es
      exitFailure

testFile :: FilePath -> ExceptT PrettyError IO ()
testFile fp = do
    p <-  liftIO (fileExist fp)
    unless p (throwE $ PrettyError $ "searched in" <+> squotes (string fp))

-- | Only test the `puppet:///` protocol (files managed by the puppet server)
--   we don't test absolute path (puppet client files)
checkFile :: FilePath -> PValue -> ExceptT PrettyError IO ()
checkFile basedir (PString f) = case T.stripPrefix "puppet:///" f of
    Just stringdir -> case T.splitOn "/" stringdir of
        ("modules":modname:rest) -> testFile (basedir <> "/modules/" <> T.unpack modname <> "/files/" <> T.unpack (T.intercalate "/" rest))
        ("files":rest)           -> testFile (basedir <> "/files/" <> T.unpack (T.intercalate "/" rest))
        ("private":_)            -> return ()
        _                        -> throwE (PrettyError $ "Invalid file source:" <+> ttext f)
    Nothing        -> return ()
-- source is always an array of possible paths. We only fails if none of them check.
checkFile basedir (PArray xs) = asum [checkFile basedir x | x <- toList xs]
checkFile _ x = throwE (PrettyError $ "Source was not a string, but" <+> pretty x)
