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
import System.Environment
import Data.List
import qualified Data.HashMap.Strict as HM
import Data.Algorithm.Diff
import qualified System.Log.Logger as LOG
import System.Exit
import Control.Monad
import Control.Monad.Error (runErrorT)
import Data.Char (toLower)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Monoid hiding (First)
import Data.Maybe (isNothing)
import Text.Parsec
import qualified Data.Vector as V
import qualified Data.Either.Strict as S

import Facter

import Puppet.PP
import Puppet.Preferences
import Puppet.Daemon
import Puppet.Interpreter.Types
import Puppet.Parser.Types
import Puppet.Parser
import Puppet.Parser.PrettyPrinter
import Puppet.Interpreter.PrettyPrinter
import Puppet.JsonCatalog
import PuppetDB.Rest
import Puppet.Testing

tshow :: Show a => a -> T.Text
tshow = T.pack . show

usage = error "Usage: puppetresource puppetdir nodename [filename]"

{-| Does all the work of initializing a daemon for querying.
Returns the final catalog when given a node name. Note that this is pretty
hackish as it will generate facts from the local computer !
-}

initializedaemonWithPuppet :: Maybe T.Text -> FilePath -> IO (T.Text -> IO (FinalCatalog, EdgeMap, FinalCatalog))
initializedaemonWithPuppet purl puppetdir = do
    LOG.updateGlobalLogger "Puppet.Daemon" (LOG.setLevel LOG.DEBUG)
    prefs <- genPreferences puppetdir
    let nprefs = case purl of
                     Nothing -> prefs
                     Just ur -> prefs { _pDBquery = pdbRequest ur }
    q <- initDaemon nprefs
    return $ \nodename -> do
        r <- allFacts nodename >>= _dGetCatalog q nodename
        case r of
            S.Left rr -> putDoc rr >> putStrLn "" >> error "error!"
            S.Right x -> return x

{-| A helper for when you don't want to use PuppetDB -}
initializedaemon :: FilePath -> IO (T.Text -> IO (FinalCatalog, EdgeMap, FinalCatalog))
initializedaemon = initializedaemonWithPuppet Nothing

parseFile :: FilePath -> IO (Either ParseError (V.Vector Statement))
parseFile fp = T.readFile fp >>= runParserT puppetParser () fp

printContent :: T.Text -> FinalCatalog -> IO ()
printContent filename catalog =
        case HM.lookup (RIdentifier "file" filename) catalog of
            Nothing -> error "File not found"
            Just r  -> case HM.lookup "content" (_rattributes r) of
                           Nothing -> error "This file has no content"
                           Just (PString c)  -> T.putStrLn c
                           Just x -> print x


main :: IO ()
main = do
    args <- getArgs
    let (rargs, puppeturl) = case args of
                             ("-r":pu:xs) -> (xs,   Just pu)
                             _            -> (args, Nothing)
    when (length rargs == 1) $ do
        p <- parseFile (head rargs)
        case p of
            Left rr -> error ("parse error:" ++ show rr)
            Right s -> putDoc (vcat (map pretty (V.toList s)))
        error "tmp"
    let (puppetdir, nodename) | (length rargs /= 2) && (length rargs /= 3) = usage
                              | otherwise = (rargs !! 0, rargs !! 1)
        getresname :: T.Text -> Maybe (T.Text, T.Text)
        getresname r =
            let isresname = T.last r == ']' && T.isInfixOf "[" r
                (rtype, rname) = T.break (== '[') r
            in if isresname
                then Just (T.map toLower rtype, T.tail $ T.init rname)
                else Nothing
        handlePrintResource resname cat
            = case getresname resname of
                Just (t,n) -> case HM.lookup (RIdentifier t n) cat of
                                  Just x -> putDoc (pretty x)
                                  Nothing -> error "Resource not found"
                Nothing    -> printContent resname cat

    queryfunc <- initializedaemonWithPuppet (fmap T.pack puppeturl) puppetdir
    printFunc <- hIsTerminalDevice stdout >>= \isterm -> return $ \x ->
        if isterm
            then putDoc x >> putStrLn ""
            else displayIO stdout (renderCompact x) >> putStrLn ""
    (x,m,e) <- queryfunc (T.pack nodename)
    if length rargs == 3
        then if (rargs !! 2) == "JSON"
                 then do
                     let json = catalog2JSon (T.pack nodename) 1 x e m
                     BSL.putStrLn json
                 else handlePrintResource (T.pack (rargs !! 2)) x
        else do
            (restest, coverage) <- testCatalog puppetdir x basicTest
            case failedTests restest of
                Just x -> printFunc (pretty x)
                Nothing -> do
                    printFunc (pretty (HM.elems x))
                    printFunc (mempty <+> dullyellow "Exported:" <+> mempty)
                    printFunc (pretty (HM.elems e))
