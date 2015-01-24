{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
-- | This is an internal module.
module Puppet.Interpreter.IO (defaultImpureMethods, interpretMonad)  where

import Puppet.PP
import Puppet.Interpreter.Types
import Puppet.Interpreter.PrettyPrinter()
import Puppet.Plugins()

import Control.Monad.Operational
import Control.Monad.RSS.Strict
import Control.Monad.State.Strict
import Control.Lens

import qualified Data.Either.Strict as S

import GHC.Stack
import Debug.Trace (traceEventIO)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Exception
import qualified Scripting.Lua as Lua
import Control.Concurrent.MVar

defaultImpureMethods :: (Functor m, MonadIO m) => ImpureMethods m
defaultImpureMethods = ImpureMethods (liftIO currentCallStack)
                                     (liftIO . file)
                                     (liftIO . traceEventIO)
                                     (\c fname args -> liftIO (runlua c fname args))
    where
        file [] = return $ Left ""
        file (x:xs) = fmap Right (T.readFile (T.unpack x)) `catch` (\SomeException{} -> file xs)
        runlua c fname args = liftIO $ withMVar c $ \lstt ->
                catch (fmap Right (Lua.callfunc lstt (T.unpack fname) args)) (\e -> return $ Left $ show (e :: SomeException))


interpretMonad :: (Functor m, Monad m)
                => InterpreterReader m
                -> InterpreterState
                -> InterpreterMonad a
                -> m (Either PrettyError a, InterpreterState, InterpreterWriter)
interpretMonad rd_ prmstate instr = case runState (viewT instr) prmstate of
                                     (!a,!nextstate) -> evalInstrGen rd_ nextstate a

    where
        evalInstrGen :: (Functor m, Monad m)
                        => InterpreterReader m
                        -> InterpreterState
                        -> ProgramViewT InterpreterInstr (State InterpreterState) a
                        -> m (Either PrettyError a, InterpreterState, InterpreterWriter)
        evalInstrGen _ stt (Return x) = return (Right x, stt, mempty)
        evalInstrGen rdr stt (a :>>= f) =
            let runC a' = interpretMonad rdr stt (f a')
                thpe = interpretMonad rdr stt . throwPosError . getError
                pdb = _pdbAPI rdr
                strFail iof errf = iof >>= \case
                    Left rr -> thpe (errf (string rr))
                    Right x -> runC x
                canFail iof = iof >>= \case
                    S.Left rr -> thpe rr
                    S.Right x -> runC x
                logStuff x c = (_3 %~ (x <>)) `fmap` c
            in  case a of
                    IsStrict                     -> runC (rdr ^. isStrict)
                    ExternalFunction fname args  -> case rdr ^. externalFunctions . at fname of
                                                        Just fn -> interpretMonad rdr stt ( fn args >>= f)
                                                        Nothing -> thpe (PrettyError ("Unknown function: " <> ttext fname))
                    GetStatement topleveltype toplevelname
                                                 -> canFail ((rdr ^. getStatement) topleveltype toplevelname)
                    ComputeTemplate fn scp cscps -> canFail ((rdr ^. computeTemplateFunction) fn scp cscps)
                    WriterTell t                 -> logStuff t (runC ())
                    WriterPass _                 -> thpe "WriterPass"
                    WriterListen _               -> thpe "WriterListen"
                    GetNativeTypes               -> runC (rdr ^. nativeTypes)
                    ErrorThrow d                 -> return (Left d, stt, mempty)
                    ErrorCatch _ _               -> thpe "ErrorCatch"
                    GetNodeName                  -> runC (rdr ^. thisNodename)
                    HieraQuery scps q t          -> canFail ((rdr ^. hieraQuery) scps q t)
                    PDBInformation               -> pdbInformation pdb >>= runC
                    PDBReplaceCatalog w          -> canFail (replaceCatalog pdb w)
                    PDBReplaceFacts fcts         -> canFail (replaceFacts pdb fcts)
                    PDBDeactivateNode nn         -> canFail (deactivateNode pdb nn)
                    PDBGetFacts q                -> canFail (getFacts pdb q)
                    PDBGetResources q            -> canFail (getResources pdb q)
                    PDBGetNodes q                -> canFail (getNodes pdb q)
                    PDBCommitDB                  -> canFail (commitDB pdb)
                    PDBGetResourcesOfNode nn q   -> canFail (getResourcesOfNode pdb nn q)
                    GetCurrentCallStack          -> (rdr ^. ioMethods . imGetCurrentCallStack) >>= runC
                    ReadFile fls                 -> strFail ((rdr ^. ioMethods . imReadFile) fls) (const $ PrettyError ("No file found in " <> list (map ttext fls)))
                    TraceEvent e                 -> (rdr ^. ioMethods . imTraceEvent) e >>= runC
                    IsIgnoredModule m            -> runC (rdr ^. ignoredModules . contains m)
                    CallLua c fname args         -> (rdr ^. ioMethods . imCallLua) c fname args >>= \case
                                                        Right x -> runC x
                                                        Left rr -> thpe (PrettyError (string rr))
