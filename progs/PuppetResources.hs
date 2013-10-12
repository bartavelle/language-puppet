{-|

Horrible and hackish ... but damn useful. This can be used as a standalone
executable or a ghci script for interactive usage. It comes with a sample Puppet
site (that I hope will gets more realistic later).

When given a single argument, it will try to parse the given file, and will
print the parsed values :

> $ puppetresources samplesite/manifests/site.pp
> node test.nod {
>     apt::builddep { Interpolable [Literal "glusterfs-server"]:
>     ;
>     }
>     ...

With two arguments, it will try to compute the catalog for a given node. The
first argument must be the path to the Puppet directory and the second the
name of the node. Note that regexp node names do not work yet.

> $ puppetresources samplesite test.nod
> The defined() function is not implemented for resource references. Returning true at "samplesite/modules/apt/manifests/ppa.pp" (line 20, column 3)
> The defined() function is not implemented for resource references. Returning true at "samplesite/modules/apt/manifests/key.pp" (line 38, column 7)
> The defined() function is not implemented for resource references. Returning true at "samplesite/modules/apt/manifests/key.pp" (line 42, column 7)
> anchor {
>     "apt::builddep::glusterfs-server": #"samplesite/modules/apt/manifests/builddep.pp" (line 12, column 12)
>         name => "apt::builddep::glusterfs-server";
>     "apt::key/Add key: 55BE302B from Apt::Source debian_unstable": #"samplesite/modules/apt/manifests/key.pp" (line 32, column 16)
>         name => "apt::key/Add key: 55BE302B from Apt::Source debian_unstable";
> ...

When adding a file name as the third argument to the previous invocation, it
will display the value of the /content/ attribute of the named /file/ resource.

> $ puppetresources samplesite test.nod karmic.pref
> The defined() function is not implemented for resource references. Returning true at "samplesite/modules/apt/manifests/ppa.pp" (line 20, column 3)
> The defined() function is not implemented for resource references. Returning true at "samplesite/modules/apt/manifests/key.pp" (line 38, column 7)
> The defined() function is not implemented for resource references. Returning true at "samplesite/modules/apt/manifests/key.pp" (line 42, column 7)
> # karmic
> Package: *
> Pin: release a=karmic
> Pin-Priority: 700

You can also just use a resource name :

> $ puppetresources samplesite test.nod 'exec[apt_update]'
> exec {
>   "apt_update": #"samplesite/modules/apt/manifests/update.pp" (line 4, column 10)
>       command     => "/usr/bin/apt-get update",
>       logoutput   => "false",
>       refreshonly => "true",
>       returns     => 0,
>       timeout     => 300,
>       tries       => 1,
>       try_sleep   => 0;
> }

With GHCI there are tons of things you can do. First initialize it

>>> queryfunc <- initializedaemon "./samplesite/"

You can now compute catalogs for various nodes.

>>> c1 <- queryfunc "test.nod"
The defined() function is not implemented for resource references. Returning true at "./samplesite//modules/apt/manifests/ppa.pp" (line 20, column 3)
The defined() function is not implemented for resource references. Returning true at "./samplesite//modules/apt/manifests/key.pp" (line 38, column 7)
The defined() function is not implemented for resource references. Returning true at "./samplesite//modules/apt/manifests/key.pp" (line 42, column 7)
>>> c2 <- queryfunc "test2.nod"
The defined() function is not implemented for resource references. Returning true at "./samplesite//modules/apt/manifests/ppa.pp" (line 20, column 3)
The defined() function is not implemented for resource references. Returning true at "./samplesite//modules/apt/manifests/key.pp" (line 38, column 7)
The defined() function is not implemented for resource references. Returning true at "./samplesite//modules/apt/manifests/key.pp" (line 42, column 7)

And you can check what the difference is between catalogs.

>>> diff c1 c2
file[karmic-updates.pref] {
# content
    + Pin-Priority: 750
    - Pin-Priority: 700
}

You also can manipulate catalogs.

>>> Map.size c1
25
>>> mapM_ print $ Map.toList $ Map.map (length . lines . (\x -> case x of (PString n) -> n) .fromJust . Map.lookup "content" . rrattributes) $ Map.filter (Map.member "content" . rrattributes) c1
(("file","debian_unstable.list"),3)
(("file","debian_unstable.pref"),4)
(("file","karmic-security.pref"),4)
(("file","karmic-updates.pref"),4)
(("file","karmic.pref"),4)

A typical usage of this tool is to compute a reference catalog, and then check the differences as you alter it. This can be done this way :

>>> reference <- queryfunc "test.nod"

And then run the following command every time you need to verify your changes are correct :

>>> queryfunc "test.nod" >>= diff reference

-}
module Main where

import System.IO
import qualified Data.HashMap.Strict as HM
import qualified System.Log.Logger as LOG
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Monoid hiding (First)
import qualified Text.Parsec as P
import qualified Data.Vector as V
import qualified Data.Either.Strict as S
import Options.Applicative as O
import Control.Monad
import Text.Regex.PCRE.String
import Data.Text.Strict.Lens
import Data.Aeson

import Facter

import Puppet.PP hiding ((<$>))
import Puppet.Preferences
import Puppet.Daemon
import Puppet.Interpreter.Types
import Puppet.Parser.Types
import Puppet.Parser
import Puppet.Parser.PrettyPrinter()
import Puppet.Interpreter.PrettyPrinter()
import Puppet.JsonCatalog
import PuppetDB.Rest
import Puppet.Testing hiding ((<$>))

tshow :: Show a => a -> T.Text
tshow = T.pack . show

{-| Does all the work of initializing a daemon for querying.
Returns the final catalog when given a node name. Note that this is pretty
hackish as it will generate facts from the local computer !
-}

initializedaemonWithPuppet :: Maybe T.Text -> FilePath -> IO (T.Text -> IO (FinalCatalog, EdgeMap, FinalCatalog), PuppetDBAPI)
initializedaemonWithPuppet purl puppetdir = do
    LOG.updateGlobalLogger "Puppet.Daemon" (LOG.setLevel LOG.DEBUG)
    prfs <- genPreferences True puppetdir
    nprefs <- case purl of
                  Nothing -> prfs
                  Just ur -> pdbConnect ur >>= \case
                    S.Right connect -> return (prfs { pdbAPI = connect })
                    S.Left  rr -> error (show rr)
    q <- initDaemon nprefs
    let f nodename = allFacts nodename
            >>= _dGetCatalog q nodename
            >>= \case
                    S.Left rr -> putDoc rr >> putStrLn "" >> error "error!"
                    S.Right x -> return x
    return (f, nprefs ^. pdbAPI)

{-| A helper for when you don't want to use PuppetDB -}
initializedaemon :: FilePath -> IO (T.Text -> IO (FinalCatalog, EdgeMap, FinalCatalog), T.Text -> Value -> IO (S.Either String Value))
initializedaemon = initializedaemonWithPuppet Nothing

parseFile :: FilePath -> IO (Either P.ParseError (V.Vector Statement))
parseFile fp = T.readFile fp >>= P.runParserT puppetParser () fp

printContent :: T.Text -> FinalCatalog -> IO ()
printContent filename catalog =
        case HM.lookup (RIdentifier "file" filename) catalog of
            Nothing -> error "File not found"
            Just r  -> case HM.lookup "content" (_rattributes r) of
                           Nothing -> error "This file has no content"
                           Just (PString c)  -> T.putStrLn c
                           Just x -> print x

data CommandLine = CommandLine { _pdb          :: Maybe String
                               , _showjson     :: Bool
                               , _showContent  :: Bool
                               , _resourceType :: Maybe T.Text
                               , _resourceName :: Maybe T.Text
                               , _puppetdir    :: FilePath
                               , _nodename     :: Maybe String
                               , _pdbfile      :: Maybe FilePath
                               } deriving Show

cmdlineParser :: Parser CommandLine
cmdlineParser = CommandLine <$> optional pdb <*> sj <*> sc <*> optional (T.pack <$> rt) <*> optional (T.pack <$> rn) <*> pdir <*> optional nn <*> optional pdbfile
    where
        sc = switch (  long "showcontent"
                    <> short 'c'
                    <> help "When specifying a file resource, only output its content (useful for testing templates)")
        sj = switch (  long "JSON"
                    <> short 'j'
                    <> help "Shows the output as a JSON document (useful for full catalog views)")
        pdb = strOption (  long "pdburl"
                        <> short 'r'
                        <> help "URL of the puppetdb (ie. http://localhost:8080/)")
        rt = strOption (  long "type"
                       <> short 't'
                       <> help "Filter the output by resource type (accepts a regular expression, ie '^file$')")
        rn = strOption (  long "name"
                       <> short 'n'
                       <> help "Filter the output by resource name (accepts a regular expression)")
        pdir = strOption (  long "puppetdir"
                         <> short 'p'
                         <> help "Puppet directory")
        nn   = strOption (  long "node"
                         <> short 'o'
                         <> help "Node name")
        pdbfile = strOption (  long "pdbfile"
                            <> short 'f'
                            <> help "Path to the testing PuppetDB file")
run :: CommandLine -> IO ()
run (CommandLine _ _ _ _ _ f Nothing _) = parseFile f >>= \case
            Left rr -> error ("parse error:" ++ show rr)
            Right s -> putDoc (vcat (map pretty (V.toList s)))
run (CommandLine puppeturl showjson showcontent mrt mrn puppetdir (Just nodename) mpdbf) = do
    (queryfunc, pdbfunc) <- initializedaemonWithPuppet (fmap T.pack puppeturl) puppetdir
    printFunc <- hIsTerminalDevice stdout >>= \isterm -> return $ \x ->
        if isterm
            then putDoc x >> putStrLn ""
            else displayIO stdout (renderCompact x) >> putStrLn ""
    (rawcatalog,m,rawexported) <- queryfunc (T.pack nodename)
    let cmpMatch Nothing _ curcat = return curcat
        cmpMatch (Just rg) lns curcat = compile compBlank execBlank (T.unpack rg) >>= \case
            Left rr -> error ("Error compiling regexp 're': "  ++ show rr)
            Right rec -> fmap HM.fromList $ filterM (filterResource lns rec) (HM.toList curcat)
        filterResource lns rec v = execute rec (v ^. lns) >>= \case
                                        Left rr -> error ("Error when applying regexp: " ++ show rr)
                                        Right Nothing -> return False
                                        _ -> return True
        filterCatalog = cmpMatch mrt (_1 . itype . unpacked) >=> cmpMatch mrn (_1 . iname . unpacked)
    catalog  <- filterCatalog rawcatalog
    exported <- filterCatalog rawexported
    case (showcontent, showjson)  of
        (_, True) -> BSL.putStrLn (catalog2JSon (T.pack nodename) 1 catalog exported m)
        (True, _) -> do
            unless (mrt == Just "file" || mrt == Nothing) (error $ "Show content only works for file, not for " ++ show mrt)
            case mrn of
                Just f -> printContent f catalog
                Nothing -> error "You should supply a resource name when using showcontent"
        _ -> do
            (restest, _) <- testCatalog puppetdir catalog basicTest
            case failedTests restest of
                Just x -> printFunc (pretty x)
                Nothing -> do
                    printFunc (pretty (HM.elems catalog))
                    unless (HM.null exported) $ do
                        printFunc (mempty <+> dullyellow "Exported:" <+> mempty)
                        printFunc (pretty (HM.elems exported))
run _ = error "Unsupported options combination"

main :: IO ()
main = execParser pinfo >>= run
    where
        pinfo :: ParserInfo CommandLine
        pinfo = ParserInfo (helper <*> cmdlineParser) True "A useful program for parsing puppet files, generating and inspecting catalogs" "puppetresources - a useful utility for dealing with Puppet" "" 3
