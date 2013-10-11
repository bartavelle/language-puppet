module Puppet.Interpreter where

import Puppet.Interpreter.Types
import Puppet.Interpreter.PrettyPrinter(containerComma)
import Puppet.Interpreter.Resolve
import Puppet.Parser.Types
import Puppet.Parser.PrettyPrinter
import Puppet.PP
import Puppet.NativeTypes

import Prelude hiding (mapM)
import Puppet.Utils
import System.Log.Logger
import Data.Maybe
import Data.List (nubBy)
import Data.Aeson hiding ((.=))
import qualified Data.Text as T
import Data.Tuple.Strict (Pair(..))
import qualified Data.Tuple.Strict as S
import qualified Data.Either.Strict as S
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import Control.Monad.Trans.RWS.Strict
import Control.Monad.Error hiding (mapM)
import Control.Lens
import qualified Data.Maybe.Strict as S
import qualified Data.Graph as G
import qualified Data.Tree as T
import Data.Foldable (toList,foldl',Foldable,foldlM)
import Data.Traversable (mapM)

-- helpers
vmapM :: (Monad m, Foldable t) => (a -> m b) -> t a -> m [b]
vmapM f = mapM f . toList

getCatalog :: ( TopLevelType -> T.Text -> IO (S.Either Doc Statement) ) -- ^ get statements function
           -> (Either T.Text T.Text -> T.Text -> Container ScopeInformation -> IO (S.Either Doc T.Text)) -- ^ compute template function
           -> (T.Text -> Value -> IO (S.Either String Value)) -- ^ puppetDB query function
           -> T.Text -- ^ Node name
           -> Facts -- ^ Facts ...
           -> Container PuppetTypeMethods -- ^ List of native types
           -> Container ( [PValue] -> InterpreterMonad PValue )
           -> IO (Pair (S.Either Doc (FinalCatalog, EdgeMap, FinalCatalog))  [Pair Priority Doc])
getCatalog gtStatement gtTemplate pdbQuery nodename facts nTypes extfuncs = do
    let rdr = InterpreterReader nTypes gtStatement gtTemplate pdbQuery extfuncs
        dummypos = initialPPos "dummy"
        initialclass = mempty & at "::" ?~ (IncludeStandard :!: dummypos)
        stt  = InterpreterState baseVars initialclass mempty ["::"] dummypos mempty [] []
        factvars = facts & each %~ (\x -> x :!: initialPPos "facts")
        callervars = ifromList [("caller_module_name", PString "::" :!: dummypos), ("module_name", PString "::" :!: dummypos)]
        baseVars = isingleton "::" (ScopeInformation (factvars <> callervars) mempty mempty (CurContainer ContRoot mempty) mempty S.Nothing)
    (output, _, warnings) <- runRWST (runErrorT (computeCatalog nodename)) rdr stt
    return (strictifyEither output :!: _warnings warnings)

isParent :: T.Text -> CurContainerDesc -> InterpreterMonad Bool
isParent _ ContRoot = return False
isParent _ (ContDefine _ _) = return False
isParent cur (ContClass possibleparent) = do
    preuse (scopes . ix cur . scopeParent) >>= \case
        Nothing -> throwPosError ("Internal error: could not find scope" <+> ttext cur <+> "possible parent" <+> ttext possibleparent)
        Just S.Nothing -> return False
        Just (S.Just p) -> if p == possibleparent
                               then return True
                               else isParent p (ContClass possibleparent)

finalize :: [Resource] -> InterpreterMonad [Resource]
finalize rlist = do
    -- step 1, apply defaults
    scp  <- getScope
    defs <- use (scopes . ix scp . scopeDefaults)
    let getOver = use (scopes . ix scp . scopeOverrides) -- retrieves current overrides
        addDefaults r = do
            let thisresdefaults = defs ^. ix (r ^. rid . itype) . defValues
            foldM (addAttribute CantReplace) r (itoList thisresdefaults)
        addOverrides r = do
            overs <- getOver
            case overs ^. at (r ^. rid) of
                Just x -> do
                    scopes . ix scp . scopeOverrides . at (r ^. rid) .= Nothing
                    addOverrides' r x
                Nothing -> return r
        addOverrides' r (ResRefOverride _ prms p) = do
            let inter = (r ^. rattributes) `HM.intersection` prms
            unless (fnull inter) $ do
                s <- getScope
                i <- isParent s (r ^. rcontainer)
                unless i $ throwPosError ("You are not allowed to override the following parameters " <+> containerComma inter <+> "already defined at" <+> showPPos p)
            return (r & rattributes %~ (<>) prms)
    withDefaults <- mapM (addOverrides >=> addDefaults) rlist
    -- There might be some overrides that could not be applied. The only
    -- valid reason is that they override something in exported resources.
    --
    -- This will probably do something unexpected on defines, but let's do
    -- it that way for now.
    let keepforlater (ResRefOverride resid resprms ropos) = resMod %= (appended : )
            where
               appended = ResourceModifier (resid ^. itype) ModifierMustMatch DontRealize (REqualitySearch "title" (PString (resid ^. iname))) overrider ropos
               overrider r = do
                   -- we must define if we can override the value
                   let canOverride = CantOverride -- TODO check inheritance
                   foldM (addAttribute canOverride) r (itoList resprms)
    fmap toList getOver >>= mapM_ keepforlater
    let expandableDefine (curstd, curdef) r = do
            n <- isNativeType (r ^. rid . itype)
            return $! if n || (r ^. rvirtuality /= Normal)
                          then (r : curstd, curdef)
                          else (curstd, r : curdef)
    (standard, defines) <- foldM expandableDefine ([], []) withDefaults
    expanded <- mapM expandDefine defines
    return $! standard ++ concat expanded

popScope :: InterpreterMonad ()
popScope = curScope %= tail

pushScope :: T.Text -> InterpreterMonad ()
pushScope s = curScope %= (s :)

evalTopLevel :: Statement -> InterpreterMonad ([Resource], Statement)
evalTopLevel (TopContainer tops s) = do
    pushScope "::"
    r <- vmapM evaluateStatement tops >>= finalize . concat
    -- popScope
    (nr, ns) <- evalTopLevel s
    popScope
    return (r <> nr, ns)
evalTopLevel x = return ([], x)

getstt :: TopLevelType -> T.Text -> InterpreterMonad ([Resource], Statement)
getstt topleveltype toplevelname = do
    -- check if this is a known class (spurious or inner class)
    use (nestedDeclarations . at (topleveltype, toplevelname)) >>= \case
        Just x -> return ([], x) -- it is known !
        Nothing -> do
            -- load the file
            getStmtfunc <- view getStatement
            liftIO (getStmtfunc topleveltype toplevelname) >>= \case
                S.Right x -> evalTopLevel x
                S.Left y  -> throwPosError y

computeCatalog :: T.Text -> InterpreterMonad (FinalCatalog, EdgeMap, FinalCatalog)
computeCatalog nodename = do
    (restop, node) <- getstt TopNode nodename
    let finalStep [] = return []
        finalStep allres = do
            -- collect stuff and apply thingies
            (realized :!: modified) <- realize allres
            -- we need to run it again against collected stuff, especially
            -- for defines that have been realized
            refinalized <- finalize (toList modified) >>= finalStep
            -- replace the modified stuff
            let res = foldl' (\curm e -> curm & at (e ^. rid) ?~ e) realized refinalized
            return (toList res)
    resnode <- evaluateNode node >>= finalStep . (++ restop)
    let (real :!: exported) = foldl' classify (mempty :!: mempty) resnode
        classify (curr :!: cure) r =
            let i curm = curm & at (r ^. rid) ?~ r
            in  case r ^. rvirtuality of
                    Normal   -> i curr :!: cure
                    Exported -> curr :!: i cure
                    ExportedRealized -> i curr :!: i cure
                    _ -> curr :!: cure
    verified <- fmap (ifromList . map (\r -> (r ^. rid, r))) $ mapM validateNativeType (toList real)
    mp <- makeEdgeMap verified
    return (verified, mp, exported)

dependencyErrors :: [T.Tree G.Vertex] -> (G.Vertex -> (RIdentifier, RIdentifier, [RIdentifier])) -> InterpreterMonad ()
dependencyErrors _ _ = throwPosError "Undefined dependency cycle"

makeEdgeMap :: FinalCatalog -> InterpreterMonad EdgeMap
makeEdgeMap ct = do
    -- merge the looaded classes and resources
    defs' <- use definedResources
    clss' <- use loadedClasses
    let defs = defs' <> classes' <> aliases' <> names'
        names' = HM.map _rpos ct
        aliases' = ifromList $ do
            r <- ct ^.. traversed . filtered (\r -> r ^. rid . iname /= r ^. ralias)
            let nrid = (r ^. rid) & iname .~ r ^. ralias
            return ( nrid, r ^. rpos)
        classes' = ifromList $ do
            (cn, _ :!: cp) <- itoList clss'
            return (RIdentifier "class" cn, cp)
    -- Preparation step : all relations to a container become relations to
    -- the stuff that's contained. We build a map of resources, stored by
    -- container.
    let containerMap :: HM.HashMap RIdentifier [RIdentifier]
        !containerMap = ifromListWith (<>) $ do
            r <- toList ct
            let toResource ContRoot = RIdentifier "class" "::"
                toResource (ContClass cn) = RIdentifier "class" cn
                toResource (ContDefine t n) = RIdentifier t n
            return (toResource (r ^. rcontainer), [r ^. rid])
        -- This function uses the previous map in order to resolve to non
        -- container resources.
        resolveDestinations :: RIdentifier -> [RIdentifier]
        resolveDestinations r = case containerMap ^. at r of
                                    Just x -> concatMap resolveDestinations x
                                    Nothing -> [r]
    -- step 1 - add relations that are stored in resources
    let reorderlink :: (RIdentifier, RIdentifier, LinkType) -> (RIdentifier, RIdentifier, LinkType)
        reorderlink (s, d, RBefore) = (d, s, RRequire)
        reorderlink (s, d, RNotify) = (d, s, RSubscribe)
        reorderlink x = x
        addRR curmap r = iunionWith (<>) curmap newmap
            where
               newmap = ifromListWith (<>) $ do
                   (rawdst, lts) <- itoList (r ^. rrelations)
                   dst <- resolveDestinations rawdst
                   lt <- toList lts
                   let (nsrc, ndst, nlt) = reorderlink (r ^. rid, dst, lt)
                   return (r ^. rid, [LinkInformation nsrc ndst nlt (r ^. rpos)])
        step1 = foldl' addRR mempty ct
    -- step 2 - add other relations (mainly stuff made from the "->"
    -- operator)
    let realign (LinkInformation s d t p) = do
            let (ns, nd, nt) = reorderlink (s, d, t)
            rs <- resolveDestinations ns
            rd <- resolveDestinations nd
            return (rs, [LinkInformation rs rd nt p])
    rels <- fmap (concatMap realign) (use extraRelations)
    let step2 = iunionWith (<>) step1 (ifromList rels)
    -- check that all resources are defined, and build graph
    let checkResDef :: (RIdentifier, [LinkInformation]) -> InterpreterMonad (RIdentifier, RIdentifier, [RIdentifier])
        checkResDef (ri, lifs) = do
            let checkExists r msg = unless (defs ^. contains r) (throwPosError msg)
                errmsg = "Unknown resource" <+> pretty ri <+> "used in the following relationships:" <+> vcat prels
                prels = [ pretty (li ^. linksrc) <+> "->" <+> pretty (li ^. linkdst) <+> showPPos (li ^. linkPos) | li <- lifs ]
            checkExists ri errmsg
            let genlnk :: LinkInformation -> InterpreterMonad RIdentifier
                genlnk lif = do
                    let d = lif ^. linkdst
                    checkExists d ("Unknown resource" <+> pretty d <+> "used in a relation at" <+> showPPos (lif ^. linkPos))
                    return d
            ds <- mapM genlnk lifs
            return (ri, ri, ds)
    (graph, gresolver) <- fmap G.graphFromEdges' $ mapM checkResDef (itoList step2)
    -- now check for scc
    let sccs = filter ((>1) . length . T.flatten) (G.scc graph)
    unless (null sccs) (dependencyErrors sccs gresolver)
    return step2

realize :: [Resource] -> InterpreterMonad (Pair FinalCatalog FinalCatalog)
realize rs = do
    let rma = ifromList (map (\r -> (r ^. rid, r)) rs)
        mutate :: Pair FinalCatalog FinalCatalog -> ResourceModifier -> InterpreterMonad (Pair FinalCatalog FinalCatalog)
        mutate (curmap :!: modified) rmod = do
            let filtrd = curmap ^.. folded . filtered fmod
                vcheck f r = f (r ^. rvirtuality)
                (isGoodvirtuality, alterVirtuality) = case rmod ^. rmType of
                                                          RealizeVirtual   -> (vcheck (/= Exported), \r -> return (r & rvirtuality .~ Normal))
                                                          RealizeCollected -> (vcheck (`elem` [Exported, ExportedRealized]), \r -> return (r & rvirtuality .~ ExportedRealized))
                                                          DontRealize      -> (vcheck (`elem` [Normal, ExportedRealized]), return)
                fmod r = (r ^. rid . itype == rmod ^. rmResType) && checkSearchExpression (rmod ^. rmSearch) r && isGoodvirtuality r
                mutation = alterVirtuality >=> rmod ^. rmMutation
                applyModification :: Pair (Pair FinalCatalog FinalCatalog) Bool -> Resource -> InterpreterMonad (Pair (Pair FinalCatalog FinalCatalog) Bool)
                applyModification (cma :!: cmo :!: matched) r = do
                    nr <- mutation r
                    let i m = m & at (nr ^. rid) ?~ nr
                    return $ if nr /= r
                                 then i cma :!: i cmo :!: True
                                 else cma :!: cmo :!: matched
            (result :!: mtch) <- foldM applyModification (curmap :!: modified :!: False) filtrd
            when (rmod ^. rmModifierType == ModifierMustMatch && not mtch) (throwError ("Could not apply this resource override :" <+> pretty rmod))
            return result
        equalModifier (ResourceModifier a1 b1 c1 d1 _ e1) (ResourceModifier a2 b2 c2 d2 _ e2) = a1 == a2 && b1 == b2 && c1 == c2 && d1 == d2 && e1 == e2
    result <- use resMod >>= foldM mutate (rma :!: mempty) . nubBy equalModifier
    resMod .= []
    return result

evaluateNode :: Statement -> InterpreterMonad [Resource]
evaluateNode (Node _ stmts inheritance p) = do
    curPos .= p
    pushScope "::"
    unless (S.isNothing inheritance) $ throwPosError "Node inheritance is not handled yet, and will probably never be"
    vmapM evaluateStatement stmts >>= finalize . concat
evaluateNode x = throwPosError ("Asked for a node evaluation, but got this instead:" <$> pretty x)

evaluateStatementsVector :: Foldable f => f Statement -> InterpreterMonad [Resource]
evaluateStatementsVector = fmap concat . vmapM evaluateStatement

-- | Converts a list of pairs into a container, checking there is no
-- duplicate
fromArgumentList :: [Pair T.Text a] -> InterpreterMonad (Container a)
fromArgumentList = foldM insertArgument mempty
    where
        insertArgument curmap (k :!: v) =
            case curmap ^. at k of
                Just _ -> throwPosError ("Parameter" <+> dullyellow (ttext k) <+> "already defined!")
                Nothing -> return (curmap & at k ?~ v)

evaluateStatement :: Statement -> InterpreterMonad [Resource]
evaluateStatement r@(ClassDeclaration cname _ _ _ _) =
    if "::" `T.isInfixOf` cname
       then nestedDeclarations . at (TopClass, cname) ?= r >> return []
       else do
           scp <- getScope
           if scp == "::"
               then nestedDeclarations . at (TopClass, cname) ?= r >> return []
               else nestedDeclarations . at (TopClass, scp <> "::" <> cname) ?= r >> return []
evaluateStatement r@(DefineDeclaration dname _ _ _) =
    if "::" `T.isInfixOf` dname
       then nestedDeclarations . at (TopDefine, dname) ?= r >> return []
       else do
           scp <- getScope
           if scp == "::"
               then nestedDeclarations . at (TopDefine, dname) ?= r >> return []
               else nestedDeclarations . at (TopDefine, scp <> "::" <> dname) ?= r >> return []
evaluateStatement r@(ResourceCollection e resType searchExp mods p) = do
    curPos .= p
    unless (fnull mods || e == Collector) (throwPosError ("It doesnt seem possible to amend attributes with an exported resource collector:" <$> pretty r))
    rsearch <- resolveSearchExpression searchExp
    let et = case e of
                 Collector -> RealizeVirtual
                 ExportedCollector -> RealizeCollected
    resMod %= (ResourceModifier resType ModifierCollector et rsearch return p : )
    return []
evaluateStatement (Dependency (t1 :!: n1) (t2 :!: n2) p) = do
    curPos .= p
    rn1 <- resolveExpressionStrings n1
    rn2 <- resolveExpressionStrings n2
    forM_ rn1 $ \an1 -> forM_ rn2 $ \an2 ->
        extraRelations %= (LinkInformation (RIdentifier t1 an1) (RIdentifier t2 an2) RBefore p :)
    return []
evaluateStatement (ResourceDeclaration rt ern eargs virt p) = do
    curPos .= p
    resnames <- resolveExpressionStrings ern
    args <- vmapM resolveArgument eargs >>= fromArgumentList
    fmap concat (mapM (\n -> registerResource rt n args virt p) resnames)
evaluateStatement (MainFunctionCall funcname funcargs p) = do
    curPos .= p
    vmapM resolveExpression funcargs >>= mainFunctionCall funcname
evaluateStatement (VariableAssignment varname varexpr p) = do
    curPos .= p
    varval <- resolveExpression varexpr
    loadVariable varname varval
    return []
evaluateStatement (ConditionalStatement conds p) = do
    curPos .= p
    let checkCond [] = return []
        checkCond ((e :!: stmts) : xs) = do
            result <- fmap pValue2Bool (resolveExpression e)
            if result
                then evaluateStatementsVector stmts
                else checkCond xs
    checkCond (toList conds)
evaluateStatement (DefaultDeclaration resType decls p) = do
    curPos .= p
    let resolveDefaultValue (prm :!: v) = fmap (prm :!:) (resolveExpression v)
    rdecls <- vmapM resolveDefaultValue decls >>= fromArgumentList
    scp <- getScope
    -- invariant that must be respected : the current scope must me create
    -- in "scopes", or nothing gets saved
    let newDefaults = ResDefaults resType scp rdecls p
        addDefaults x = scopes . ix scp . scopeDefaults . at resType ?= x
        -- default merging with parent
        mergedDefaults curdef = newDefaults & defValues .~ (rdecls <> (curdef ^. defValues))
    preuse (scopes . ix scp . scopeDefaults . ix resType) >>= \case
        Nothing -> addDefaults newDefaults
        Just de -> if de ^. defSrcScope == scp
                       then throwPosError ("Defaults for resource" <+> ttext resType <+> "already declared at" <+> showPPos (de ^. defPos))
                       else addDefaults (mergedDefaults de)
    return []
evaluateStatement (ResourceOverride rt urn eargs p) = do
    curPos .= p
    raassignements <- vmapM resolveArgument eargs >>= fromArgumentList
    rn <- resolveExpressionString urn
    scp <- getScope
    curoverrides <- use (scopes . ix scp . scopeOverrides)
    let rident = RIdentifier rt rn
    withAssignements <- case curoverrides ^. at rident of
                            Just (ResRefOverride _ prevass prevpos) -> do
                                let cm = prevass `HM.intersection` raassignements
                                unless (fnull cm) (throwPosError ("The following parameters were already overriden at" <+> showPPos prevpos <+> ":" <+> containerComma cm))
                                return (prevass <> raassignements)
                            Nothing -> return raassignements
    scopes . ix scp . scopeOverrides . at rident ?= ResRefOverride rident withAssignements p
    return []
evaluateStatement r = throwError ("Do not know how to evaluate this statement:" <$> pretty r)

-----------------------------------------------------------
-- Class evaluation
-----------------------------------------------------------

loadVariable :: T.Text -> PValue -> InterpreterMonad ()
loadVariable varname varval = do
    scp <- getScope
    p <- use curPos
    scopeDefined <- use (scopes . contains scp)
    variableDefined <- preuse (scopes . ix scp . scopeVariables . ix varname)
    case (scopeDefined, variableDefined) of
        (False, _) -> throwPosError ("Internal error: trying to save a variable in unknown scope" <+> ttext scp)
        (_, Just (_ :!: pp)) -> throwPosError ("Variable" <+> pretty (UVariableReference varname) <+> "already defined at" <+> showPPos pp)
        _ -> scopes . ix scp . scopeVariables . at varname ?= (varval :!: p)

loadParameters :: Foldable f => Container PValue -> f (Pair T.Text (S.Maybe Expression)) -> PPosition -> InterpreterMonad ()
loadParameters params classParams defaultPos = do
    let !classParamSet     = HS.fromList (map S.fst (toList classParams))
        !mandatoryParamSet = HS.fromList (map S.fst (classParams ^.. folded . filtered (S.isNothing . S.snd)))
        !definedParamSet   = ikeys params
        !unsetParams       = mandatoryParamSet `HS.difference` definedParamSet
        !spuriousParams    = definedParamSet `HS.difference` classParamSet
    unless (fnull unsetParams) $ throwPosError ("The following mandatory parameters where not set:" <+> tupled (map ttext $ toList unsetParams))
    unless (fnull spuriousParams) $ throwPosError ("The following parameters are unknown:" <+> tupled (map (dullyellow . ttext) $ toList spuriousParams))
    let isDefault = not . flip HS.member definedParamSet . S.fst
    mapM_ (uncurry loadVariable) (itoList params)
    curPos .= defaultPos
    forM_ (filter isDefault (toList classParams)) $ \(k :!: v) -> do
        rv <- case v of
                  S.Nothing -> throwPosError "Internal error: invalid invariant at loadParameters"
                  S.Just e -> resolveExpression e
        loadVariable k rv

-- | Enters a new scope, checks it is not already defined, and inherits de
-- defaults from the current scope
--
-- Inheriting the defaults is necessary for non native types, because they
-- will be expanded in "finalize"
enterScope :: S.Maybe T.Text -> CurContainerDesc -> InterpreterMonad T.Text
enterScope parent cont = do
    let scopename = case cont of
                        ContRoot -> "::"
                        ContClass x -> x
                        ContDefine dt dn -> "#define/" <> dt <> "/" <> dn
    scopeAlreadyDefined <- use (scopes . contains scopename)
    when scopeAlreadyDefined (throwPosError ("Internal error: scope already defined when loading scope for" <+> pretty cont))
    scp <- getScope
    -- TODO fill tags
    basescope <- case parent of
        S.Nothing -> do
            curdefs <- use (scopes . ix scp . scopeDefaults)
            return $ ScopeInformation mempty curdefs mempty (CurContainer cont mempty) mempty parent
        S.Just p -> do
            parentscope <- use (scopes . at p)
            when (isNothing parentscope) (throwPosError ("Internal error: could not find parent scope" <+> ttext p))
            let Just psc = parentscope
            return (psc & scopeParent .~ parent)
    scopes . at scopename ?= basescope
    return scopename

expandDefine :: Resource -> InterpreterMonad [Resource]
expandDefine r = do
    let deftype = r ^. rid . itype
        defname = r ^. rid . iname
        modulename = case T.splitOn "::" deftype of
                         [] -> deftype
                         (x:_) -> x
    curcaller <- resolveVariable "module_name"
    scopename <- enterScope S.Nothing (ContDefine deftype defname)
    (spurious, dls) <- getstt TopDefine deftype
    case dls of
        (DefineDeclaration _ defineParams stmts cp) -> do
            p <- use curPos
            curPos .= r ^. rpos
            pushScope scopename
            loadVariable "title" (PString defname)
            loadVariable "name" (PString defname)
            -- not done through loadvariable because of override
            -- errors
            scopes . ix scopename . scopeVariables . at "module_name" ?= (PString modulename :!: p)
            scopes . ix scopename . scopeVariables . at "callermodule_name" ?= (curcaller :!: p)
            loadParameters (r ^. rattributes) defineParams cp
            curPos .= cp
            res <- evaluateStatementsVector stmts
            out <- finalize (spurious ++ res)
            popScope
            return out
        _ -> throwPosError ("Internal error: we did not retrieve a DefineDeclaration, but had" <+> pretty dls)

loadClass :: T.Text
          -> Container PValue
          -> ClassIncludeType
          -> InterpreterMonad [Resource]
loadClass classname params cincludetype = do
    p <- use curPos
    -- check if the class has already been loaded
    -- http://docs.puppetlabs.com/puppet/3/reference/lang_classes.html#using-resource-like-declarations
    use (loadedClasses . at classname) >>= \case
        Just (_ :!: pp) -> do
            when (cincludetype == IncludeResource) (throwPosError ("Can't include class" <+> ttext classname <+> "twice when using the resource-like syntax (first occurance at" <+> showPPos pp <> ")"))
            return []
        -- already loaded, go on
        Nothing -> do
            loadedClasses . at classname ?= (cincludetype :!: p)
            -- load the actual class, note we are not changing the current position
            -- right now
            (spurious, cls) <- getstt TopClass classname
            case cls of
                (ClassDeclaration _ classParams inh stmts cp) -> do
                    -- check if we need to define a resource representing the class
                    -- This will be the case for the first standard include
                    inhstmts <- case inh of
                                    S.Nothing -> return []
                                    S.Just ihname -> loadClass ihname mempty IncludeResource
                    scopename <- enterScope inh (ContClass classname)
                    classresource <- if cincludetype == IncludeStandard
                                         then do
                                             scp <- use curScope
                                             return [Resource (RIdentifier "class" classname) classname mempty mempty scp ContRoot Normal mempty p]
                                         else return []
                    pushScope scopename
                    let modulename = case T.splitOn "::" classname of
                                         [] -> classname
                                         (x:_) -> x
                    -- not done through loadvariable because of override
                    -- errors
                    scopes . ix scopename . scopeVariables . at "module_name" ?= (PString modulename :!: p)
                    loadParameters params classParams cp
                    curPos .= cp
                    res <- evaluateStatementsVector stmts
                    out <- finalize (classresource ++ spurious ++ inhstmts ++ res)
                    popScope
                    return out
                _ -> throwPosError ("Internal error: we did not retrieve a ClassDeclaration, but had" <+> pretty cls)
-----------------------------------------------------------
-- Resource stuff
-----------------------------------------------------------

addRelationship :: LinkType -> PValue -> Resource -> InterpreterMonad Resource
addRelationship lt (PResourceReference dt dn) r = return (r & rrelations %~ insertLt)
    where
        insertLt = iinsertWith (<>) (RIdentifier dt dn) (mempty & contains lt .~ True)
addRelationship lt (PArray vals) r = foldlM (flip (addRelationship lt)) r vals
addRelationship _ PUndef r = return r
addRelationship _ notrr _ = throwPosError ("Expected a resource reference, not:" <+> pretty notrr)

addTagResource :: Resource -> T.Text -> Resource
addTagResource r rv = r & rtags . contains rv .~ True

addAttribute :: OverrideType -> Resource -> (T.Text, PValue) -> InterpreterMonad Resource
addAttribute _ r ("alias", v) = fmap (\rv -> r & ralias .~ rv) (resolvePValueString v)
addAttribute _ r ("audit", _) = use curPos >>= \p -> warn ("Metaparameter audit ignored at" <+> showPPos p) >> return r
addAttribute _ r ("noop", _) = use curPos >>= \p -> warn ("Metaparameter noop ignored at" <+> showPPos p) >> return r
addAttribute _ r ("loglevel", _) = use curPos >>= \p -> warn ("Metaparameter loglevel ignored at" <+> showPPos p) >> return r
addAttribute _ r ("schedule", _) = use curPos >>= \p -> warn ("Metaparameter schedule ignored at" <+> showPPos p) >> return r
addAttribute _ r ("stage", _) = use curPos >>= \p -> warn ("Metaparameter stage ignored at" <+> showPPos p) >> return r
addAttribute _ r ("tag", PArray v) = foldM (\cr cv -> fmap (addTagResource cr) (resolvePValueString cv)) r (toList v)
addAttribute _ r ("tag", v) = fmap (addTagResource r) (resolvePValueString v)
addAttribute _ r ("before", d) = addRelationship RBefore d r
addAttribute _ r ("notify", d) = addRelationship RNotify d r
addAttribute _ r ("require", d) = addRelationship RRequire d r
addAttribute _ r ("subscribe", d) = addRelationship RSubscribe d r
addAttribute b r (t,v) = case (r ^. rattributes . at t, b) of
                             (_, Replace)     -> return (r & rattributes . at t ?~ v)
                             (Nothing, _)     -> return (r & rattributes . at t ?~ v)
                             (_, CantReplace) -> return r
                             _                -> do
                                 -- we must check if the resource scope is
                                 -- a parent of the current scope
                                 curscope <- getScope
                                 i <- isParent curscope (r ^. rcontainer)
                                 if i
                                     then return (r & rattributes . at t ?~ v)
                                     else throwPosError ("Attribute" <+> ttext t <+> "defined multiple times for" <+> pretty (r ^. rid) <+> showPPos (r ^. rpos))

registerResource :: T.Text -> T.Text -> Container PValue -> Virtuality -> PPosition -> InterpreterMonad [Resource]
registerResource "class" _ _ Virtual p  = curPos .= p >> throwPosError "Cannot declare a virtual class (or perhaps you can, but I do not know what this means)"
registerResource "class" _ _ Exported p = curPos .= p >> throwPosError "Cannot declare an exported class (or perhaps you can, but I do not know what this means)"
registerResource rt rn arg vrt p = do
    curPos .= p
    scp <- getScope
    CurContainer cnt tgs <- {-# SCC "rrGetContainer" #-}
        preuse (scopes . ix scp . scopeContainer) >>= \case
            Just x -> return x
            Nothing -> throwPosError "Internal error: can't find the current container at registerResource"
    -- default tags
    -- http://docs.puppetlabs.com/puppet/3/reference/lang_tags.html#automatic-tagging
    -- http://docs.puppetlabs.com/puppet/3/reference/lang_tags.html#containment
    let !defaulttags = {-# SCC "rrGetTags" #-} HS.fromList (rt : classtags) <> tgs
        allsegs x = x : T.splitOn "::" x
        !classtags = case cnt of
                        ContRoot        -> []
                        ContClass cn    -> allsegs cn
                        ContDefine dt _ -> allsegs dt
    allScope <- use curScope
    let baseresource = Resource (RIdentifier rt rn) rn mempty mempty allScope cnt vrt defaulttags p
    r <- foldM (addAttribute CantOverride) baseresource (itoList arg)
    let resid = RIdentifier rt rn
    case rt of
        "class" -> {-# SCC "rrClass" #-} do
            definedResources . at resid ?= p
            fmap (r:) $ loadClass rn (r ^. rattributes) IncludeResource
        _ -> {-# SCC "rrGeneralCase" #-}
            use (definedResources . at resid) >>= \case
                Just apos -> throwPosError ("Resource" <+> pretty resid <+> "already defined at" <+> showPPos apos)
                Nothing -> do
                    definedResources . at resid ?= p
                    return [r]

-- A helper function for the various loggers
logWithModifier :: Priority -> (Doc -> Doc) -> [PValue] -> InterpreterMonad [Resource]
logWithModifier prio m [t] = do
    p <- use curPos
    rt <- resolvePValueString t
    logWriter prio (m (ttext rt) <+> showPPos p)
    return []
logWithModifier _ _ _ = throwPosError "This function takes a single argument"

-- functions : this can't really be exported as it uses a lot of stuff from
-- this module ...
mainFunctionCall :: T.Text -> [PValue] -> InterpreterMonad [Resource]
mainFunctionCall "showscope" _ = use curScope >>= warn . list . map ttext >> return []
-- The logging functions
mainFunctionCall "alert" a   = logWithModifier ALERT        red         a
mainFunctionCall "crit" a    = logWithModifier CRITICAL     red         a
mainFunctionCall "debug" a   = logWithModifier DEBUG        dullwhite   a
mainFunctionCall "emerg" a   = logWithModifier EMERGENCY    red         a
mainFunctionCall "err" a     = logWithModifier ERROR        dullred     a
mainFunctionCall "info" a    = logWithModifier INFO         green       a
mainFunctionCall "notice" a  = logWithModifier NOTICE       white       a
mainFunctionCall "warning" a = logWithModifier WARNING      dullyellow  a
mainFunctionCall "include" includes =
    fmap concat $ forM includes $ \e -> do
        classname <- resolvePValueString e
        loadClass classname mempty IncludeStandard
mainFunctionCall "create_resources" [rtype, hs] = mainFunctionCall "create_resources" [rtype, hs, PHash mempty]
mainFunctionCall "create_resources" [PString rtype, PHash hs, PHash defs] = do
    p <- use curPos
    let genRes (rname, PHash rargs) = registerResource rtype rname (rargs <> defs) Normal p
        genRes (rname, x) = throwPosError ("create_resource(): the value corresponding to key" <+> ttext rname <+> "should be a hash, not" <+> pretty x)
    fmap concat (mapM genRes (itoList hs))
mainFunctionCall "create_resources" args = throwPosError ("create_resource(): expects between two and three arguments, of type [string,hash,hash], and not:" <+> pretty args)
mainFunctionCall "realize" args = do
    p <- use curPos
    let realiz (PResourceReference rt rn) = resMod %= (ResourceModifier rt ModifierMustMatch RealizeVirtual (REqualitySearch "title" (PString rn)) return p : )
        realiz x = throwPosError ("realize(): all arguments must be resource references, not" <+> pretty x)
    mapM_ realiz args >> return []
mainFunctionCall "tag" args = do
    scp <- getScope
    let addTag x = scopes . ix scp . scopeExtraTags . contains x .= True
    mapM_ (resolvePValueString >=> addTag) args
    return []
mainFunctionCall "fail" [x] = fmap (("fail:" <+>) . dullred . ttext) (resolvePValueString x) >>= throwPosError
mainFunctionCall "fail" _ = throwPosError "fail(): This function takes a single argument"
mainFunctionCall fname args = do
    p <- use curPos
    let representation = MainFunctionCall fname mempty p
    external <- view externalFunctions
    rs <- case external ^. at fname of
        Just f -> f args
        Nothing -> throwPosError ("Unknown function:" <+> pretty representation)
    unless (rs == PUndef) $ throwPosError ("This function call should return" <+> pretty PUndef <+> "and not" <+> pretty rs <$> pretty representation)
    return []

