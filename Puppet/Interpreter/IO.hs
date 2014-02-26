{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Puppet.Interpreter.IO where

import Puppet.PP
import Puppet.Interpreter.Types
import Puppet.Plugins()

import Control.Monad.Operational
import Control.Monad.Error
import Control.Monad.RSS.Strict
import Control.Monad.State.Strict
import Control.Lens
import Control.Exception
import Control.Concurrent.MVar

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString as BS
import qualified Scripting.Lua as Lua

import qualified Data.Either.Strict as S
import Data.Maybe (isJust)
import GHC.Stack
import Debug.Trace (traceEventIO)
import Text.Regex.PCRE.ByteString
import Text.Regex.PCRE.ByteString.Utils

canfailIO :: IO (S.Either Doc a) -> ErrorT Doc (RSST InterpreterReader InterpreterWriter InterpreterState IO) a
canfailIO m = liftIO (eitherDocIO m) >>= \case
    S.Right x -> return x
    S.Left y -> throwPosError y

bs :: BS.ByteString -> Doc
bs = string . show

interpretIO :: InterpreterReader
            -> InterpreterMonad a
            -> RSMIO a
interpretIO rdr = intexpr . viewT
    where
        pdb = _pdbAPI rdr
        runC :: RSMIO b -> (b -> InterpreterMonad a) -> RSMIO a
        runC c f = do
            o <- c
            interpretIO rdr (f o)
        intexpr :: State InterpreterState (ProgramViewT InterpreterInstr (State InterpreterState) a) -> RSMIO a
        intexpr computation = do
            initstate <- get
            case runState computation initstate of
                (!a,!nextstate) -> put nextstate >> evalInstr a
        evalInstr :: ProgramViewT InterpreterInstr (State InterpreterState) a -> RSMIO a
        evalInstr (Return x) = return x
        evalInstr (WriterTell t :>>= f) = tell t >> runC (return ()) f
        evalInstr (WriterPass m :>>= f) = runC (pass (interpretIO rdr m)) f
        evalInstr (WriterListen m :>>= f) = runC (listen (interpretIO rdr m)) f
        evalInstr (GetNativeTypes :>>= f) = interpretIO rdr (f (rdr ^. nativeTypes))
        evalInstr (ExternalFunction fname args :>>= f) = case rdr ^. externalFunctions . at fname of
                                                                Just fn -> interpretIO rdr ( fn args >>= f)
                                                                Nothing -> throwPosError ("Unknown function: " <> ttext fname)
        evalInstr (ErrorThrow d :>>= _) = throwError d
        evalInstr (ErrorCatch m h :>>= f) = runC (catchError (interpretIO rdr m) (interpretIO rdr . h)) f
        evalInstr (GetNodeName :>>= f) = interpretIO rdr (f (rdr ^. thisNodename))

        evalInstr (GetStatement topleveltype toplevelname :>>= f) = runC (canfailIO ((rdr ^. getStatement) topleveltype toplevelname)) f
        evalInstr (ComputeTemplate fn scp cscps :>>= f) = runC (canfailIO ((rdr ^. computeTemplateFunction) fn scp cscps)) f
        evalInstr (HieraQuery scps q t :>>= f) = runC (canfailIO ((rdr ^. hieraQuery) scps q t)) f
        evalInstr (GetCurrentCallStack :>>= f) = runC (liftIO currentCallStack) f
        evalInstr (ReadFile fls :>>= f) = runC (canfailIO (file fls)) f
            where
                file :: [T.Text] -> IO (S.Either Doc T.Text)
                file [] = return $ S.Left ("No file found in" <+> list (map ttext fls))
                file (x:xs) = fmap S.Right (T.readFile (T.unpack x)) `catch` (\SomeException{} -> file xs)
        evalInstr (TraceEvent e :>>= f) = runC (liftIO (traceEventIO e)) f
        evalInstr (PDBInformation :>>= f) = runC (liftIO (pdbInformation pdb)) f
        evalInstr (PDBReplaceCatalog w :>>= f) = runC (canfailIO (replaceCatalog pdb w)) f
        evalInstr (PDBReplaceFacts fcts :>>= f) = runC (canfailIO (replaceFacts pdb fcts)) f
        evalInstr (PDBDeactivateNode nn :>>= f) = runC (canfailIO (deactivateNode pdb nn)) f
        evalInstr (PDBGetFacts q :>>= f) = runC (canfailIO (getFacts pdb q)) f
        evalInstr (PDBGetResources q :>>= f) = runC (canfailIO (getResources pdb q)) f
        evalInstr (PDBGetNodes q :>>= f) = runC (canfailIO (getNodes pdb q)) f
        evalInstr (PDBCommitDB :>>= f) = runC (canfailIO (commitDB pdb)) f
        evalInstr (PDBGetResourcesOfNode nn q :>>= f) = runC (canfailIO (getResourcesOfNode pdb nn q)) f
        evalInstr (SubstituteCompile regexp target replacement :>>= f) = liftIO (substituteCompile regexp target replacement) >>= \case
            Left rr -> throwPosError ("regsubst" <> parens (bs regexp <> comma <> bs replacement) <> ":" <+> string rr)
            Right x -> interpretIO rdr (f x)
        evalInstr (SplitCompile splt src :>>= f) = liftIO (splitCompile splt src) >>= \case
            Left rr -> throwPosError ("split" <> parens (bs splt <> comma <> bs src) <> ":" <+> string rr)
            Right x -> interpretIO rdr (f x)
        evalInstr (Compile c e r :>>= f) = liftIO (compile c e r) >>= \case
            Left rr -> throwPosError ("compile" <> parens (bs r) <> ":" <+> string (show rr))
            Right x -> interpretIO rdr (f x)
        evalInstr (Execute rv va :>>= f) = liftIO (execute rv va) >>= \case
            Left rr -> throwPosError ("execute" <> parens ("/regexp/" <> comma <>  bs va) <> ":" <+> string (show rr))
            Right x -> interpretIO rdr $ f (isJust x )
        evalInstr (CallLua c fname args :>>= f) = runC runlua f
            where
                runlua = do
                    r <- liftIO $ withMVar c $ \stt ->
                        catch (fmap Right (Lua.callfunc stt (T.unpack fname) args)) (\e -> return $ Left $ show (e :: SomeException))
                    case r of
                        Right x -> return x
                        Left rr -> throwPosError (string rr)

