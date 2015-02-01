{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}
module Puppet.OptionalTests (testCatalog) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.Error
import           Data.Foldable                    hiding (forM_, mapM_)
import           Data.Maybe                       (mapMaybe)
import           Data.Monoid                      ((<>))
import qualified Data.Text                        as T
import           Prelude                          hiding (all, notElem)
import           Puppet.Interpreter.PrettyPrinter ()
import           Puppet.Interpreter.Types
import           Puppet.PP                        hiding ((<$>))
import           System.Posix.Files

-- | Entry point for all optional tests
testCatalog :: FilePath -> FinalCatalog -> IO ()
testCatalog = testFileSources

-- | Test source for every file resources in the catalog.
testFileSources :: FilePath -> FinalCatalog -> IO ()
testFileSources basedir c = do
    let getFiles = filter presentFile . toList
        presentFile r | r ^. rid . itype /= "file" = False
                      | (r ^. rattributes . at "ensure") `notElem` [Nothing, Just "present"] = False
                      | r ^. rattributes . at "source" == Just PUndef = False
                      | otherwise = True
        getSource = mapMaybe (\r -> (,) `fmap` pure r <*> r ^. rattributes . at "source")
    let files = (getSource . getFiles) c
    forM_ files $ \(_, filesource) -> do
        rs <- runErrorT (checkFile basedir filesource)
        case rs of
             Right () -> return ()
             Left err -> fail (show err)


testFile :: FilePath -> ErrorT PrettyError IO ()
testFile fp = liftIO (fileExist fp) >>= (`unless` (throwError $ PrettyError $ "Searched in" <+> string fp))

checkFile :: FilePath -> PValue -> ErrorT PrettyError IO ()
checkFile basedir (PString f) = case (T.stripPrefix "puppet:///" f, T.stripPrefix "file:///" f) of
    (Just stringdir, _) -> case T.splitOn "/" stringdir of
        ("modules":modname:rest) -> testFile (basedir <> "/modules/" <> T.unpack modname <> "/files/" <> T.unpack (T.intercalate "/" rest))
        ("files":rest)           -> testFile (basedir <> "/files/" <> T.unpack (T.intercalate "/" rest))
        ("private":_)            -> return ()
        _                        -> throwError (PrettyError $ "Invalid file source:" <+> ttext f)
    (Nothing, _)        -> return ()
checkFile basedir pv@(PArray xs) = asum [checkFile basedir x | x <- toList xs] <|> throwError (PrettyError $ "Could not find the file in" <+> pretty pv)
checkFile _ x = throwError (PrettyError $ "Source was not a string, but" <+> pretty x)
