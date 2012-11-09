{-| This module is used for user plugins. It exports three functions that should
be easy to use: 'initLua', 'puppetFunc' and 'closeLua'. Right now it is used by
the "Puppet.Daemon" by initializing and destroying the Lua context for each
catalog computation. Obviously such plugins will be implemented in Lua.

Users plugins are right now limited to custom functions. The user must put them
at the exact same place as their Ruby counterparts, except the extension must be
lua instead of rb. In the file, a function called by the same name that takes a
single argument must be defined. This argument will be an array made of all the
functions arguments. If the file doesn't parse, it will be silently ignored.

Here are the things that must be kept in mind:

* Lua doesn't have integers. All numbers are double.

* All Lua associative arrays that are returned must have a "simple" type for all
the keys, as it will be converted to a string. Numbers will be directly
converted and other types will produce strange results.

* This currently only works for functions that must return a value. They will
have no access to the manifests data.
-}
module Puppet.Plugins (initLua, puppetFunc, closeLua, getFiles, getBasename) where

import Prelude hiding (catch)
import qualified Scripting.Lua as Lua
import Scripting.LuaUtils()
import System.Directory
import Control.Exception
import Data.String.Utils (endswith)
import qualified Data.Map as Map
import Control.Monad.IO.Class

import Puppet.Interpreter.Types
import Puppet.Printers

instance Lua.StackValue ResolvedValue
    where
        push l (ResolvedString s)        = Lua.push l s
        push l (ResolvedRegexp s)        = Lua.push l s
        push l (ResolvedInt i)           = Lua.push l (fromIntegral i :: Int)
        push l (ResolvedDouble d)        = Lua.push l d
        push l (ResolvedBool b)          = Lua.push l b
        push l (ResolvedRReference rr _) = Lua.push l rr
        push l (ResolvedArray arr)       = Lua.push l arr
        push l (ResolvedHash h)          = Lua.push l (Map.fromList h)
        push l (ResolvedUndefined)       = Lua.push l "undefined"

        peek l n = do
            t <- Lua.ltype l n
            case t of
                Lua.TBOOLEAN -> fmap (fmap ResolvedBool) (Lua.peek l n)
                Lua.TSTRING  -> fmap (fmap ResolvedString) (Lua.peek l n)
                Lua.TNUMBER  -> fmap (fmap ResolvedDouble) (Lua.peek l n)
                Lua.TNIL     -> return (Just ResolvedUndefined)
                Lua.TNONE    -> return (Just ResolvedUndefined)
                Lua.TTABLE   -> do
                    p <- Lua.peek l n :: IO (Maybe (Map.Map ResolvedValue ResolvedValue))
                    case p of
                        Just kp -> let ks = Map.keys kp
                                       cp = map (\(a,b) -> (showValue a, b)) $ Map.toList kp
                                   in  if (all (\(a,b) -> a == ResolvedDouble b) (zip ks [1.0..]))
                                       -- horrible trick to check whether we are being returned a list or a hash
                                       -- this will probably fail somehow
                                        then return $ Just (ResolvedArray (map snd cp))
                                        else return $ Just (ResolvedHash cp)
                        _ -> return Nothing
                _ -> return Nothing

        valuetype _ = Lua.TUSERDATA

getDirContents :: FilePath -> IO [FilePath]
getDirContents x = fmap (filter (not . all (=='.'))) (getDirectoryContents x)

-- find files in subdirectories
checkForSubFiles :: String -> String -> IO [String]
checkForSubFiles extension dir = do
    content <- catch (fmap Right (getDirContents dir)) (\e -> return $ Left (e :: IOException))
    case content of
        Right o -> do
            return ((map (\x -> dir ++ "/" ++ x) . filter (endswith extension)) o )
        Left _ -> return []

-- Find files in the module directory that are in a module subdirectory and
-- finish with a specific extension
getFiles :: String -> String -> String -> IO [String]
getFiles moduledir subdir extension =
        getDirContents moduledir
        >>= mapM ( (checkForSubFiles extension) . (\x -> moduledir ++ "/" ++ x ++ "/" ++ subdir))
        >>= return . concat

getLuaFiles :: String -> IO [String]
getLuaFiles moduledir = getFiles moduledir "lib/puppet/parser/functions" ".lua"

-- retrieves the base name of a file (very slow and naïve implementation).
getBasename :: String -> String
getBasename fullname =
    let lastpart = reverse $ fst $ break (=='/') $ reverse fullname
    in  fst $ break (=='.') lastpart

loadLuaFile :: Lua.LuaState -> String -> IO [String]
loadLuaFile l file = do
    r <- Lua.loadfile l file
    case r of
        0 -> Lua.call l 0 0 >> return [getBasename file]
        _ -> return []
{-| Runs a puppet function in the 'CatalogMonad' monad. It takes a state,
function name and list of arguments. It returns a valid Puppet value.
-}
puppetFunc :: Lua.LuaState -> String -> [ResolvedValue] -> CatalogMonad ResolvedValue
puppetFunc l fn args = do
    content <- liftIO $ catch (fmap Right (Lua.callfunc l fn args)) (\e -> return $ Left $ show (e :: SomeException))
    case content of
        Right x -> return x
        Left  y -> throwPosError y

-- | Initializes the Lua state. The argument is the modules directory. Each
-- subdirectory will be traversed for functions.
-- The default location is @\/lib\/puppet\/parser\/functions@.
initLua :: String -> IO (Lua.LuaState, [String])
initLua moduledir = do
    funcfiles <- getLuaFiles moduledir
    l <- Lua.newstate
    Lua.openlibs l
    luafuncs <- fmap concat $ mapM (loadLuaFile l) funcfiles
    return (l , luafuncs)

-- | Obviously releases the Lua state.
closeLua :: Lua.LuaState -> IO ()
closeLua = Lua.close
