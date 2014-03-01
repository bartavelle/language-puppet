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
import GHC.Stack
import Debug.Trace (traceEventIO)
import Text.Regex.PCRE.ByteString
import Text.Regex.PCRE.ByteString.Utils

bs :: BS.ByteString -> Doc
bs = string . show

interpretIO :: InterpreterReader
            -> InterpreterState
            -> InterpreterMonad a
            -> IO (Either Doc a, InterpreterState, InterpreterWriter)
interpretIO rdr prmstate instr = case runState (viewT instr) prmstate of
                                     (!a,!nextstate) -> evalInstr nextstate a
    where
        pdb = _pdbAPI rdr
        evalInstr :: InterpreterState -> ProgramViewT InterpreterInstr (State InterpreterState) a -> IO (Either Doc a, InterpreterState, InterpreterWriter)
        evalInstr stt (Return x) = return (Right x, stt, mempty)
        evalInstr stt (a :>>= f) =
            let runC a' = interpretIO rdr stt (f a')
                err rs = return (Left rs, stt, mempty)
                canFailIO iof = iof >>= \case
                    S.Left rr -> err rr
                    S.Right x -> runC x
                runlua c fname args = liftIO $ withMVar c $ \lstt ->
                        catch (fmap Right (Lua.callfunc lstt (T.unpack fname) args)) (\e -> return $ Left $ show (e :: SomeException))
                strFailIO iof errf = iof >>= \case
                    Left rr -> err (errf (string rr))
                    Right x -> runC x
                file :: [T.Text] -> IO (Either String T.Text)
                file [] = return $ Left ""
                file (x:xs) = fmap Right (T.readFile (T.unpack x)) `catch` (\SomeException{} -> file xs)
            in  case a of
                    ExternalFunction fname args  -> case rdr ^. externalFunctions . at fname of
                                                        Just fn -> interpretIO rdr stt ( fn args >>= f)
                                                        Nothing -> err ("Unknown function: " <> ttext fname)
                    GetStatement topleveltype toplevelname
                                                 -> canFailIO ((rdr ^. getStatement) topleveltype toplevelname)
                    ComputeTemplate fn scp cscps -> canFailIO ((rdr ^. computeTemplateFunction) fn scp cscps)
                    WriterTell t                 -> (_3 %~ (t <>)) `fmap` runC ()
                    WriterPass _                 -> error "WriterPass"
                    WriterListen _               -> error "WriterListen"
                    GetNativeTypes               -> runC (rdr ^. nativeTypes)
                    ErrorThrow d                 -> err d
                    ErrorCatch _ _               -> error "ErrorCatch"
                    GetNodeName                  -> runC (rdr ^. thisNodename)
                    HieraQuery scps q t          -> canFailIO ((rdr ^. hieraQuery) scps q t)
                    GetCurrentCallStack          -> liftIO currentCallStack >>= runC
                    ReadFile fls                 -> strFailIO (file fls) (const $ "No file found in " <> list (map ttext fls))
                    TraceEvent e                 -> traceEventIO e >>= runC
                    PDBInformation               -> pdbInformation pdb >>= runC
                    PDBReplaceCatalog w          -> canFailIO (replaceCatalog pdb w)
                    PDBReplaceFacts fcts         -> canFailIO (replaceFacts pdb fcts)
                    PDBDeactivateNode nn         -> canFailIO (deactivateNode pdb nn)
                    PDBGetFacts q                -> canFailIO (getFacts pdb q)
                    PDBGetResources q            -> canFailIO (getResources pdb q)
                    PDBGetNodes q                -> canFailIO (getNodes pdb q)
                    PDBCommitDB                  -> canFailIO (commitDB pdb)
                    PDBGetResourcesOfNode nn q   -> canFailIO (getResourcesOfNode pdb nn q)
                    SplitCompile splt src        -> strFailIO (splitCompile splt src) (\rr -> "split" <> parens (bs splt <> comma <> bs src) <> ":" <+> rr)
                    SubstituteCompile regexp target replacement
                                                 -> strFailIO (substituteCompile regexp target replacement) (\rr -> "regsubst" <> parens (bs regexp <> comma <> bs replacement) <> ":" <+> rr)
                    Compile c e r                -> strFailIO ((_Left %~ show) `fmap` compile c e r) (\rr -> "compile" <> parens (bs r) <> ":" <+> rr)
                    Execute rv va                -> strFailIO (((_Left %~ show) . (_Right %~ has _Just)) `fmap` execute rv va) (\rr -> "execute" <> parens ("/regexp/" <> comma <>  bs va) <> ":" <+> rr)
                    CallLua c fname args         -> runlua c fname args >>= \case
                                                        Right x -> runC x
                                                        Left rr -> interpretIO rdr stt (throwPosError (string rr))

