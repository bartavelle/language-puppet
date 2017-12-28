{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}

-- | This is an internal module.
module Puppet.Interpreter.IO (
    interpretMonad
  ) where

import           XPrelude

import           Control.Monad.Operational
import           Control.Monad.State.Strict
import qualified Data.Either.Strict               as S
import qualified Data.Text                        as Text

import           Hiera.Server
import           Puppet.Interpreter.PrettyPrinter ()
import           Puppet.Interpreter.Types
import           PuppetDB


-- | The operational interpreter function
interpretMonad :: Monad m
               => InterpreterReader m
               -> InterpreterState
               -> InterpreterMonad a
               -> m (Either PrettyError a, InterpreterState, InterpreterWriter)
interpretMonad r s0 instr = let (!p, !s1) = runState (viewT instr) s0
                            in eval r s1 p

-- The internal (not exposed) eval function
eval :: Monad m
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
            Left rr -> thpe (errf (ppstring rr))
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
                                                Nothing -> thpe (PrettyError ("Unknown function: " <> ppline fname))
            GetStatement topleveltype toplevelname
                                         -> canFail ((r ^. readerGetStatement) topleveltype toplevelname)
            ComputeTemplate fn stt       -> canFail ((r ^. readerGetTemplate) fn stt r)
            WriterTell t                 -> logStuff t (runInstr ())
            WriterPass _                 -> thpe "WriterPass"
            WriterListen _               -> thpe "WriterListen"
            PuppetPaths                  -> runInstr (r ^. readerPuppetPaths)
            RebaseFile                   -> runInstr (r ^. readerRebaseFile)
            GetNativeTypes               -> runInstr (r ^. readerNativeTypes)
            ErrorThrow d                 -> return (Left d, s, mempty)
            GetNodeName                  -> runInstr (r ^. readerNodename)
            HieraQuery scps q t          -> canFail (queryHiera (r ^. readerHieraQuery) scps q t)
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
            ReadFile fls                 -> strFail ((r ^. readerIoMethods . ioReadFile) fls) (const $ PrettyError ("No file found in " <> list (map ppline fls)))
            TraceEvent e                 -> (r ^. readerIoMethods . ioTraceEvent) e >>= runInstr
            IsIgnoredModule m            -> runInstr (r ^. readerIgnoredModules . contains m)
            IsExternalModule m           -> runInstr (r ^. readerExternalModules . contains m)
            -- on error, the program state is RESET and the logged messages are dropped
            ErrorCatch atry ahandle      -> do
                (eres, s', w) <- interpretMonad r s atry
                case eres of
                    Left rr -> interpretMonad r s (ahandle rr >>= k)
                    Right x -> logStuff w (interpretMonad r s' (k x))


-- | Query hiera layers
queryHiera :: Monad m =>  HieraQueryLayers m -> Container Text -> Text -> HieraQueryType -> m (S.Either PrettyError (Maybe PValue))
queryHiera layers scps q t = do
  val <- (layers^.globalLayer) scps q t
  case val of
    S.Right Nothing -> do
      let
        modname =
          case Text.splitOn "::" (Text.dropWhile (==':') q) of
            []    -> Nothing
            [_]   -> Nothing
            (m:_) -> Just m
        layer = modname >>= (\n -> layers ^.moduleLayer.at n)
      maybe (pure val) (\hq -> hq scps q t) layer
    _ -> pure val
