{-# LANGUAGE LambdaCase #-}
-- | The module works in IO and throws a 'PrettyError' exception at each failure.
-- These exceptions can be caught (see the exceptions package).
module Puppet.OptionalTests (testCatalog) where

import           Control.Lens
import           Control.Monad                    (unless)
import           Control.Monad.Catch
import           Control.Monad.Trans              (liftIO)
import           Control.Monad.Trans.Except
import           Data.Foldable                    (asum, toList, traverse_)
import qualified Data.HashSet                     as HS
import           Data.Maybe                       (mapMaybe)
import           Data.Monoid                      ((<>))
import qualified Data.Text                        as T

import           Puppet.Interpreter.PrettyPrinter ()
import           Puppet.Interpreter.Types
import           Puppet.Lens                      (_PString)
import           Puppet.PP
import           Puppet.Preferences
import           System.Posix.Files


-- | Entry point for all optional tests
testCatalog :: Preferences IO -> FinalCatalog -> IO ()
testCatalog prefs c = testFileSources (prefs ^. prefPuppetPaths.baseDir) c >> testUsersGroups (prefs ^. prefKnownusers) (prefs ^. prefKnowngroups) c

-- | Tests that all users and groups are defined
testUsersGroups :: [T.Text] -> [T.Text] -> FinalCatalog -> IO ()
testUsersGroups kusers kgroups c = do
    let users = HS.fromList $ "" : "0" : map (view (rid . iname)) (getResourceFrom "user") ++ kusers
        groups = HS.fromList $ "" : "0" : map (view (rid . iname)) (getResourceFrom "group") ++ kgroups
        checkResource lu lg = mapM_ (checkResource' lu lg)
        checkResource' lu lg res = do
            let msg att name = align (vsep [ "Resource" <+> ttext (res^.rid.itype)
                                             <+> ttext (res^.rid.iname) <+> showPos (res^.rpos._1)
                                           , "reference the unknown" <+> string att <+> squotes (ttext name)])
                               <> line
            case lu of
                Just lu' -> do
                    let u = res ^. rattributes . lu' . _PString
                    unless (HS.member u users) $ throwM $ PrettyError (msg "user" u)
                Nothing -> pure ()
            case lg of
                Just lg' -> do
                    let g = res ^. rattributes . lg' . _PString
                    unless (HS.member g groups) $ throwM $ PrettyError (msg "group" g)
                Nothing -> pure ()
    do
        checkResource (Just $ ix "owner") (Just $ ix "group") (getResourceFrom "file")
        checkResource (Just $ ix "user")  (Just $ ix "group") (getResourceFrom "exec")
        checkResource (Just $ ix "user")  Nothing             (getResourceFrom "cron")
        checkResource (Just $ ix "user")  Nothing             (getResourceFrom "ssh_authorized_key")
        checkResource (Just $ ix "user")  Nothing             (getResourceFrom "ssh_authorized_key_secure")
        checkResource Nothing             (Just $ ix "gid")   (getResourceFrom "users")
    where
      getResourceFrom t = c ^.. traverse . filtered (\r -> r ^. rid . itype == t && r ^. rattributes . at "ensure" /= Just "absent")

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
    go [] es = traverse_ throwM es

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
