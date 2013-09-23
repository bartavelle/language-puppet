{-# OPTIONS_GHC -fno-warn-orphans #-}

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
converted and other types will produce strange results. (currently this doesn't work at all, all associative arrays will be turned into lists, ignoring the keys)

* This currently only works for functions that must return a value. They will
have no access to the manifests data.

-}
module Puppet.Plugins (initLua, puppetFunc, closeLua, getFiles) where

import Puppet.PP
import qualified Scripting.Lua as Lua
import Scripting.LuaUtils()
import Control.Exception
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import System.IO
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import Control.Monad.IO.Class

import Puppet.Interpreter.Types
import Puppet.Utils

instance Lua.StackValue PValue
    where
        push l (PString s)               = Lua.push l s
        push l (PBoolean b)              = Lua.push l b
        push l (PResourceReference rr _) = Lua.push l rr
        push l (PArray arr)              = Lua.push l (V.toList arr)
        push l (PHash m)                 = Lua.push l (Map.fromList $ HM.toList m)
        push l (PUndef)                  = Lua.push l ("undefined" :: T.Text)

        peek l n = do
            Lua.ltype l n >>= \case
                Lua.TBOOLEAN -> fmap (fmap PBoolean) (Lua.peek l n)
                Lua.TSTRING  -> fmap (fmap PString) (Lua.peek l n)
                Lua.TNUMBER  -> fmap (fmap (PString . tshow)) (Lua.peek l n :: IO (Maybe Double))
                Lua.TNIL     -> return (Just PUndef)
                Lua.TNONE    -> return (Just PUndef)
                Lua.TTABLE   -> fmap (fmap (PArray . V.fromList)) (Lua.peek l n)
                _ -> return Nothing

        valuetype _ = Lua.TUSERDATA

getDirContents :: T.Text -> IO [T.Text]
getDirContents x = fmap (filter (not . T.all (=='.'))) (getDirectoryContents x)

-- find files in subdirectories
checkForSubFiles :: T.Text -> T.Text -> IO [T.Text]
checkForSubFiles extension dir =
    catch (fmap Right (getDirContents dir)) (\e -> return $ Left (e :: IOException)) >>= \case
        Right o -> return ((map (\x -> dir <> "/" <> x) . filter (T.isSuffixOf extension)) o )
        Left _ -> return []

-- Find files in the module directory that are in a module subdirectory and
-- finish with a specific extension
getFiles :: T.Text -> T.Text -> T.Text -> IO [T.Text]
getFiles moduledir subdir extension = fmap concat $
    getDirContents moduledir
        >>= mapM ( checkForSubFiles extension . (\x -> moduledir <> "/" <> x <> "/" <> subdir))

getLuaFiles :: T.Text -> IO [T.Text]
getLuaFiles moduledir = getFiles moduledir "lib/puppet/parser/luafunctions" ".lua"

loadLuaFile :: Lua.LuaState -> T.Text -> IO [T.Text]
loadLuaFile l file =
    Lua.loadfile l (T.unpack file) >>= \case
        0 -> Lua.call l 0 0 >> return [takeBaseName file]
        _ -> do
            T.hPutStrLn stderr ("Could not load file " <> file)
            return []
{-| Runs a puppet function in the 'CatalogMonad' monad. It takes a state,
function name and list of arguments. It returns a valid Puppet value.
-}
puppetFunc :: Lua.LuaState -> T.Text -> [PValue] -> InterpreterMonad PValue
puppetFunc l fn args =
    liftIO ( catch (fmap Right (Lua.callfunc l (T.unpack fn) args)) (\e -> return $ Left $ show (e :: SomeException)) ) >>= \case
        Right x -> return x
        Left  y -> throwPosError (string y)

-- | Initializes the Lua state. The argument is the modules directory. Each
-- subdirectory will be traversed for functions.
-- The default location is @\/lib\/puppet\/parser\/functions@.
initLua :: T.Text -> IO (Lua.LuaState, [T.Text])
initLua moduledir = do
    funcfiles <- getLuaFiles moduledir
    l <- Lua.newstate
    Lua.openlibs l
    luafuncs <- fmap concat $ mapM (loadLuaFile l) funcfiles
    return (l , luafuncs)

-- | Obviously releases the Lua state.
closeLua :: Lua.LuaState -> IO ()
closeLua = Lua.close
