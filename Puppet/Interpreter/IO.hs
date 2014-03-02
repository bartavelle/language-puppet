{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
-- | This is an internal module.
module Puppet.Interpreter.IO where

import Puppet.PP
import Puppet.Interpreter.Types
import Puppet.Plugins()

import Control.Monad.Operational
import Control.Monad.RSS.Strict
import Control.Monad.State.Strict
import Control.Lens

import qualified Data.ByteString as BS
import qualified Data.Either.Strict as S

import Text.Regex.PCRE.ByteString
import Text.Regex.PCRE.ByteString.Utils
import GHC.Stack
import Debug.Trace (traceEventIO)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Exception
import qualified Scripting.Lua as Lua
import Control.Concurrent.MVar

bs :: BS.ByteString -> Doc
bs = string . show

defaultImpureMethods :: (Functor m, MonadIO m) => ImpureMethods m
defaultImpureMethods = ImputeMethods (liftIO currentCallStack)
                                     (liftIO . file)
                                     (liftIO . traceEventIO)
                                     (\a b c -> liftIO (substituteCompile a b c))
                                     (\a b -> liftIO (splitCompile a b))
                                     (\a b c -> (_Left %~ show) `fmap` liftIO (compile a b c))
                                     (\rv va -> liftIO (((_Left %~ show) . (_Right %~ has _Just)) `fmap` execute rv va))
                                     (\c fname args -> liftIO (runlua c fname args))
    where
        file [] = return $ Left ""
        file (x:xs) = fmap Right (T.readFile (T.unpack x)) `catch` (\SomeException{} -> file xs)
        runlua c fname args = liftIO $ withMVar c $ \lstt ->
                catch (fmap Right (Lua.callfunc lstt (T.unpack fname) args)) (\e -> return $ Left $ show (e :: SomeException))

evalInstrGen :: (Functor m, Monad m) => InterpreterReader m -> InterpreterState -> ProgramViewT InterpreterInstr (State InterpreterState) a -> m (Either Doc a, InterpreterState, InterpreterWriter)
evalInstrGen _ stt (Return x) = return (Right x, stt, mempty)
evalInstrGen rdr stt (a :>>= f) =
    let runC a' = interpretMonad rdr stt (f a')
        thpe = interpretMonad rdr stt . throwPosError
        pdb = _pdbAPI rdr
        strFail iof errf = iof >>= \case
            Left rr -> thpe (errf (string rr))
            Right x -> runC x
        canFail iof = iof >>= \case
            S.Left rr -> thpe rr
            S.Right x -> runC x
    in  case a of
            ExternalFunction fname args  -> case rdr ^. externalFunctions . at fname of
                                                Just fn -> interpretMonad rdr stt ( fn args >>= f)
                                                Nothing -> thpe ("Unknown function: " <> ttext fname)
            GetStatement topleveltype toplevelname
                                         -> canFail ((rdr ^. getStatement) topleveltype toplevelname)
            ComputeTemplate fn scp cscps -> canFail ((rdr ^. computeTemplateFunction) fn scp cscps)
            WriterTell t                 -> (_3 %~ (t <>)) `fmap` runC ()
            WriterPass _                 -> thpe "WriterPass"
            WriterListen _               -> thpe "WriterListen"
            GetNativeTypes               -> runC (rdr ^. nativeTypes)
            ErrorThrow d                 -> thpe d
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
            ReadFile fls                 -> strFail ((rdr ^. ioMethods . imReadFile) fls) (const $ "No file found in " <> list (map ttext fls))
            TraceEvent e                 -> (rdr ^. ioMethods . imTraceEvent) e >>= runC
            SplitCompile splt src        -> strFail ((rdr ^. ioMethods . imSplitCompile) splt src) (\rr -> "split" <> parens (bs splt <> comma <> bs src) <> ":" <+> rr)
            SubstituteCompile regexp target replacement
                                         -> strFail ((rdr ^. ioMethods . imSubstituteCompile) regexp target replacement) (\rr -> "regsubst" <> parens (bs regexp <> comma <> bs replacement) <> ":" <+> rr)
            Compile c e r                -> strFail ((_Left %~ show) `fmap` (rdr ^. ioMethods . imCompile) c e r) (\rr -> "compile" <> parens (bs r) <> ":" <+> rr)
            Execute rv va                -> strFail ((rdr ^. ioMethods . imExecute) rv va) (\rr -> "execute" <> parens ("/regexp/" <> comma <>  bs va) <> ":" <+> rr)
            CallLua c fname args         -> (rdr ^. ioMethods . imCallLua) c fname args >>= \case
                                                Right x -> runC x
                                                Left rr -> thpe (string rr)


interpretMonad :: (Functor m, Monad m)
                => InterpreterReader m
                -> InterpreterState
                -> InterpreterMonad a
                -> m (Either Doc a, InterpreterState, InterpreterWriter)
interpretMonad rd_ prmstate instr = case runState (viewT instr) prmstate of
                                     (!a,!nextstate) -> evalInstrGen rd_ nextstate a
