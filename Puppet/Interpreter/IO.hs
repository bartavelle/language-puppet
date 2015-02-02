{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
-- | This is an internal module.
module Puppet.Interpreter.IO (defaultImpureMethods, interpretMonad)  where

import Puppet.PP hiding ((<$>))
import Puppet.Interpreter.Types
import Puppet.Interpreter.PrettyPrinter()
import Puppet.Plugins()

import Control.Monad.Operational
import Control.Monad.State.Strict
import Control.Lens
import Control.Applicative

import qualified Data.Either.Strict as S

import Data.Monoid
import GHC.Stack
import Debug.Trace (traceEventIO)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Scripting.Lua as Lua
import Control.Exception
import Control.Concurrent.MVar

defaultImpureMethods :: (Functor m, MonadIO m) => ImpureMethods m
defaultImpureMethods = ImpureMethods (liftIO currentCallStack)
                                     (liftIO . file)
                                     (liftIO . traceEventIO)
                                     (\c fname args -> liftIO (runlua c fname args))
    where
        file [] = return $ Left ""
        file (x:xs) = (Right <$> T.readFile (T.unpack x)) `catch` (\SomeException{} -> file xs)
        runlua c fname args = liftIO $ withMVar c $ \lstt ->
                catch (Right <$> Lua.callfunc lstt (T.unpack fname) args) (\e -> return $ Left $ show (e :: SomeException))


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
        pdb = r^.pdbAPI
        strFail iof errf = iof >>= \case
            Left rr -> thpe (errf (string rr))
            Right x -> runInstr x
        canFail iof = iof >>= \case
            S.Left err -> thpe err
            S.Right x -> runInstr x
        logStuff x c = (_3 %~ (x <>)) <$> c
    in  case a of
            IsStrict                     -> runInstr (r ^. isStrict)
            ExternalFunction fname args  -> case r ^. externalFunctions . at fname of
                                                Just fn -> interpretMonad r s ( fn args >>= k)
                                                Nothing -> thpe (PrettyError ("Unknown function: " <> ttext fname))
            GetStatement topleveltype toplevelname
                                         -> canFail ((r ^. getStatement) topleveltype toplevelname)
            ComputeTemplate fn scp cscps -> canFail ((r ^. computeTemplateFunction) fn scp cscps)
            WriterTell t                 -> logStuff t (runInstr ())
            WriterPass _                 -> thpe "WriterPass"
            WriterListen _               -> thpe "WriterListen"
            GetNativeTypes               -> runInstr (r ^. nativeTypes)
            ErrorThrow d                 -> return (Left d, s, mempty)
            ErrorCatch _ _               -> thpe "ErrorCatch"
            GetNodeName                  -> runInstr (r ^. thisNodename)
            HieraQuery scps q t          -> canFail ((r ^. hieraQuery) scps q t)
            PDBInformation               -> pdbInformation pdb >>= runInstr
            PDBReplaceCatalog w          -> canFail (replaceCatalog pdb w)
            PDBReplaceFacts fcts         -> canFail (replaceFacts pdb fcts)
            PDBDeactivateNode nn         -> canFail (deactivateNode pdb nn)
            PDBGetFacts q                -> canFail (getFacts pdb q)
            PDBGetResources q            -> canFail (getResources pdb q)
            PDBGetNodes q                -> canFail (getNodes pdb q)
            PDBCommitDB                  -> canFail (commitDB pdb)
            PDBGetResourcesOfNode nn q   -> canFail (getResourcesOfNode pdb nn q)
            GetCurrentCallStack          -> (r ^. ioMethods . imGetCurrentCallStack) >>= runInstr
            ReadFile fls                 -> strFail ((r ^. ioMethods . imReadFile) fls) (const $ PrettyError ("No file found in " <> list (map ttext fls)))
            TraceEvent e                 -> (r ^. ioMethods . imTraceEvent) e >>= runInstr
            IsIgnoredModule m            -> runInstr (r ^. ignoredModules . contains m)
            CallLua c fname args         -> (r ^. ioMethods . imCallLua) c fname args >>= \case
                                                Right x -> runInstr x
                                                Left rr -> thpe (PrettyError (string rr))
