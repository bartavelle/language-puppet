{-# LANGUAGE GADTs #-}

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
    in
    case a of
      IsStrict                     -> runInstr (r ^. readerIsStrict)
      ExternalFunction name args  ->
        -- #271: namespace is currently ignored when looking up puppetlabs functions
        let (nsp, name') = Text.breakOnEnd "::" name
        in
        case r ^. readerExternalFunc . at name' of
          Just fn -> interpretMonad r s ( fn args >>= k)
          Nothing -> thpe (PrettyError ("Unknown function: (" <> ppline nsp <> ")" <> ppline name'))
      GetStatement toptype topname -> canFail ((r ^. readerGetStatement) toptype topname)
      ComputeTemplate src st       -> canFail ((r ^. readerGetTemplate) src st r)
      WriterTell t                 -> logStuff t (runInstr ())
      WriterPass _                 -> thpe "WriterPass"
      WriterListen _               -> thpe "WriterListen"
      PuppetPaths                  -> runInstr (r ^. readerPuppetPaths)
      Facts                        -> runInstr (r ^. readerFacts)
      RebaseFile                   -> runInstr (r ^. readerRebaseFile)
      GetNativeTypes               -> runInstr (r ^. readerNativeTypes)
      ErrorThrow d                 -> return (Left d, s, mempty)
      GetNodeName                  -> runInstr (r ^. readerNodename)
      HieraQuery scps q t          ->
        runExceptT (queryHiera (r ^. readerHieraQuery) scps q t) >>= either thpe runInstr
      PDBInformation               -> pdbInformation pdb >>= runInstr
      PDBReplaceCatalog w          -> canFailX (replaceCatalog pdb w)
      PDBReplaceFacts fcts         -> canFailX (replaceFacts pdb fcts)
      PDBDeactivateNode nn         -> canFailX (deactivateNode pdb nn)
      PDBGetFacts q                -> canFailX (getPDBFacts pdb q)
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

-- query all hiera layers
queryHiera :: Monad m
           => HieraQueryLayers m
           -> Container PValue
           -> Text
           -> HieraQueryType
           -> ExceptT PrettyError m (Maybe PValue)
queryHiera layers scps q t = do
  eglobal <- (layers^.globalLayer) scps q t
  eenvironment <- (layers ^.environmentLayer) scps q t
  let modname =
        case Text.splitOn "::" (Text.dropWhile (==':') q) of
          []    -> Nothing
          [_]   -> Nothing
          (m:_) -> Just m
      layer = modname >>= (\n -> layers ^.moduleLayer.at n)
  emodle <- maybe (pure Nothing) (\hq -> hq scps q t) layer
  case catMaybes [eglobal, eenvironment, emodle] of
    [] -> pure Nothing
    x:xs -> Just <$> foldM (mergeWith t) x xs

