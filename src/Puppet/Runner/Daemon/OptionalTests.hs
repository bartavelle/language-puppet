-- | The module accumulates 'PrettyError's in the ExceptT monad transformer.
module Puppet.Runner.Daemon.OptionalTests (testCatalog) where

import           XPrelude

import qualified Data.HashSet              as Set
import qualified Data.Text                 as Text
import qualified System.Directory          as Directory

import           Puppet.Language
import           Puppet.Runner.Preferences


-- | Entry point for all optional tests
testCatalog :: Preferences IO
            -> FinalCatalog
            -> IO (Either PrettyError ())
testCatalog prefs c =
  runExceptT
    $  testFileSources (prefs ^. prefPuppetPaths.baseDir) c
    *> testUsersGroups (prefs ^. prefKnownusers) (prefs ^. prefKnowngroups) c

-- | Tests that all users and groups are defined
testUsersGroups :: [Text] -> [Text] -> FinalCatalog -> ExceptT PrettyError IO ()
testUsersGroups kusers kgroups c = do
  let users = Set.fromList $ "" : "0" : map (view (rid . iname)) (getResourceFrom "user") ++ kusers
      groups = Set.fromList $ "" : "0" : map (view (rid . iname)) (getResourceFrom "group") ++ kgroups
      checkResource lu lg = mapM_ (checkResource' lu lg)
      checkResource' lu lg res = do
          let msg att name = align (vsep [ "Resource" <+> ppline (res^.rid.itype)
                                           <+> ppline (res^.rid.iname) <+> showPos (res^.rpos._1)
                                         , "references the unknown" <+> att <+> squotes (ppline name)])
                             <> line
          case lu of
              Just lu' -> do
                  let u = res ^. rattributes . lu' . _PString
                  unless (Set.member u users) $ throwE $ PrettyError (msg "user" u)
              Nothing -> pure ()
          case lg of
              Just lg' -> do
                  let g = res ^. rattributes . lg' . _PString
                  unless (Set.member g groups) $ throwE $ PrettyError (msg "group" g)
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
testFileSources :: FilePath -> FinalCatalog -> ExceptT PrettyError IO ()
testFileSources basedir c = do
    let getfiles = filter presentFile . toList
        presentFile r = r ^. rid . itype == "file"
                        && (r ^. rattributes . at "ensure") `elem` [Nothing, Just "present"]
                        && r ^. rattributes . at "source" /= Just PUndef
        recurse r = r ^? rattributes . ix "recurse" . _PBoolean == Just True
        getsource = mapMaybe (\r -> (,,) <$> pure r <*> r ^. rattributes . at "recurse" <*> pure (recurse r))
    checkAllSources basedir $ (getsource . getfiles) c

-- | Check source for all file resources and append failures along.
checkAllSources :: FilePath -> [(Resource, PValue, Bool)] -> ExceptT PrettyError IO ()
checkAllSources fp fs =
  -- we could just do :
  -- traverse_ (\(res, src) -> catchE (checkFile fp src) (throwE ...)) fs
  -- but that would print the first encountered failure.
  go fs []
  where
    go :: [(Resource, PValue, Bool)] -> [PrettyError] -> ExceptT PrettyError IO ()
    go ((res, filesrc, recurse):xs) es = ExceptT $ do
      runExceptT (checkFile fp filesrc recurse) >>= \case
        Right () -> runExceptT $ go xs es
        Left err ->
          runExceptT
          $ go xs ((PrettyError $ align (vsep [ "Could not find" <+> pretty filesrc
                                              , getError err
                                              , showPos (res^.rpos^._1)
                                              ])):es)
    go [] [] = pure ()
    go [] es = throwE (mconcat es)

testFile :: Bool -> FilePath -> ExceptT PrettyError IO ()
testFile recurse fp = do
    p <-  liftIO (Directory.doesFileExist fp)
    p' <- if recurse && not p
            then liftIO (Directory.doesDirectoryExist fp)
            else return p
    unless p' (throwE $ PrettyError $ "searched in" <+> squotes (pptext fp))

-- | Only test the `puppet:///` protocol (files managed by the puppet server)
--   we don't test absolute path (puppet client files)
checkFile :: FilePath -> PValue -> Bool -> ExceptT PrettyError IO ()
checkFile basedir (PString f) recurse =
  case Text.stripPrefix "puppet:///" f of
    Just stringdir -> case Text.splitOn "/" stringdir of
        ("modules":modname:rest) -> testFile recurse (basedir <> "/modules/" <> toS modname <> "/files/" <> toS (Text.intercalate "/" rest))
        ("files":rest)           -> testFile recurse (basedir <> "/files/" <> toS (Text.intercalate "/" rest))
        ("private":_)            -> pure ()
        _                        -> throwE (PrettyError $ "Invalid file source:" <+> ppline f)
    Nothing        -> return ()
-- source is always an array of possible paths. We only fails if none of them check.
checkFile basedir (PArray xs) recurse  = asum [checkFile basedir x recurse | x <- toList xs]
checkFile _ x _ = throwE (PrettyError $ "Source was not a string, but" <+> pretty x)
