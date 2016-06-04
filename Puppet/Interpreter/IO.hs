{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}

-- | This is an internal module.
module Puppet.Interpreter.IO (
    defaultImpureMethods
  , interpretMonad
  ) where

import           Control.Concurrent.MVar
import           Control.Exception
import           Control.Lens
import           Control.Monad.Operational
import           Control.Monad.State.Strict
import           Control.Monad.Trans.Except
import qualified Data.Either.Strict               as S
import           Data.Monoid
import qualified Data.Text                        as T
import qualified Data.Text.IO                     as T
import           Debug.Trace                      (traceEventIO)
import           GHC.Stack

import           Puppet.Interpreter.PrettyPrinter ()
import           Puppet.Interpreter.Types
import           Puppet.PP

defaultImpureMethods :: (Functor m, MonadIO m) => IoMethods m
defaultImpureMethods = IoMethods (liftIO currentCallStack)
                                     (liftIO . file)
                                     (liftIO . traceEventIO)
    where
        file [] = return $ Left ""
        file (x:xs) = (Right <$> T.readFile (T.unpack x)) `catch` (\SomeException{} -> file xs)


-- | The operational interpreter function
interpretMonad :: (Functor m, Monad m)
                => InterpreterReader m
                -> InterpreterState
                -> InterpreterMonad a
                -> m (Either PrettyError a, InterpreterState, InterpreterWriter)
interpretMonad r s0 instr = let (!p, !s1) = runState (viewT instr) s0
                            in eval r s1 p

-- The internal (not exposed) eval function
eval :: (Functor m, Monad m)
                => InterpreterReader m
                -> InterpreterState
                -> ProgramViewT InterpreterInstr (State InterpreterState) a
                -> m (Either PrettyError a, InterpreterState, InterpreterWriter)
eval _ s (Return x) = return (Right x, s, mempty)
eval r s (a :>>= k) =
    let runInstr = interpretMonad r s . k -- run one instruction
        thpe = interpretMonad r s . throwPosError . getError
        pdb = r^.readerPdbApi
        strFail iof errf = iof >>= \case
            Left rr -> thpe (errf (string rr))
            Right x -> runInstr x
        canFail iof = iof >>= \case
            S.Left err -> thpe err
            S.Right x -> runInstr x
        canFailX iof = runExceptT iof >>= \case
            Left err -> thpe err
            Right x -> runInstr x
        logStuff x c = (_3 %~ (x <>)) <$> c
    in  case a of
            IsStrict                     -> runInstr (r ^. readerIsStrict)
            ExternalFunction fname args  -> case r ^. readerExternalFunc . at fname of
                                                Just fn -> interpretMonad r s ( fn args >>= k)
                                                Nothing -> thpe (PrettyError ("Unknown function: " <> ttext fname))
            GetStatement topleveltype toplevelname
                                         -> canFail ((r ^. readerGetStatement) topleveltype toplevelname)
            ComputeTemplate fn stt       -> canFail ((r ^. readerGetTemplate) fn stt r)
            WriterTell t                 -> logStuff t (runInstr ())
            WriterPass _                 -> thpe "WriterPass"
            WriterListen _               -> thpe "WriterListen"
            PuppetPaths                  -> runInstr (r ^. readerPuppetPaths)
            GetNativeTypes               -> runInstr (r ^. readerNativeTypes)
            ErrorThrow d                 -> return (Left d, s, mempty)
            ErrorCatch _ _               -> thpe "ErrorCatch"
            GetNodeName                  -> runInstr (r ^. readerNodename)
            HieraQuery scps q t          -> canFail ((r ^. readerHieraQuery) scps q t)
            PDBInformation               -> pdbInformation pdb >>= runInstr
            PDBReplaceCatalog w          -> canFailX (replaceCatalog pdb w)
            PDBReplaceFacts fcts         -> canFailX (replaceFacts pdb fcts)
            PDBDeactivateNode nn         -> canFailX (deactivateNode pdb nn)
            PDBGetFacts q                -> canFailX (getFacts pdb q)
            PDBGetResources q            -> canFailX (getResources pdb q)
            PDBGetNodes q                -> canFailX (getNodes pdb q)
            PDBCommitDB                  -> canFailX (commitDB pdb)
            PDBGetResourcesOfNode nn q   -> canFailX (getResourcesOfNode pdb nn q)
            GetCurrentCallStack          -> (r ^. readerIoMethods . ioGetCurrentCallStack) >>= runInstr
            ReadFile fls                 -> strFail ((r ^. readerIoMethods . ioReadFile) fls) (const $ PrettyError ("No file found in " <> list (map ttext fls)))
            TraceEvent e                 -> (r ^. readerIoMethods . ioTraceEvent) e >>= runInstr
            IsIgnoredModule m            -> runInstr (r ^. readerIgnoredModules . contains m)
            IsExternalModule m           -> runInstr (r ^. readerExternalModules . contains m)
