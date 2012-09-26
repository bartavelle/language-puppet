module Puppet.Plugins (initLua) where

import Prelude hiding (catch)
import qualified Scripting.Lua as Lua
import Scripting.LuaUtils
import System.Directory
import Control.Exception
import Data.String.Utils (endswith)
import qualified Data.Map as Map
import Control.Monad
import Data.Maybe (fromJust)
import Control.Monad.Loops (whileM)


instance (Lua.StackValue a, Lua.StackValue b, Ord a) => Lua.StackValue (Map.Map a b)
    where
        push l mp = do
            let llen = Map.size mp + 1
            Lua.createtable l llen 0
            forM_ (Map.toList mp) $ \(key, val) -> do
                Lua.push    l key
                Lua.push    l val
                Lua.rawset  l (-3)
        peek l i = do
            top <- Lua.gettop l
            let ix = if (i < 0) then top + i + 1 else i
            Lua.pushnil l
            arr <- whileM (Lua.next l ix) $ do
                xk <- Lua.peek l (-2)
                xv <- Lua.peek l (-1)
                Lua.pop l 1
                return (fromJust xk, fromJust xv)
            return $ Just (Map.fromList arr)
        valuetype _ = Lua.TTABLE


getDirContents :: FilePath -> IO [FilePath]
getDirContents x = fmap (filter (not . all (=='.'))) (getDirectoryContents x)

checkForLuaFiles :: String -> IO [String]
checkForLuaFiles dir = do
    let funcdir = dir ++ "/lib/puppet/parser/functions"
    content <- catch (fmap Right (getDirContents funcdir)) (\e -> return $ Left (e :: IOException))
    case content of
        Right o -> do
            return ((map (\x -> funcdir ++ "/" ++ x) . filter (endswith ".lua")) o )
        Left _ -> return []

getLuaFiles :: String -> IO [String]
getLuaFiles moduledir = do
    getDirContents moduledir >>= mapM (checkForLuaFiles . (\x -> moduledir ++ "/" ++ x)) >>= return . concat

loadLuaFile :: Lua.LuaState -> String -> IO [String]
loadLuaFile l file = do
    let gbasename :: String -> String
        gbasename fullname =
            let lastpart = reverse $ fst $ break (=='/') $ reverse fullname
            in  fst $ break (=='.') lastpart
    r <- Lua.loadfile l file
    case r of
        0 -> Lua.call l 0 0 >> return [gbasename file]
        _ -> return []

initLua :: String -> IO ()
initLua moduledir = do
    funcfiles <- getLuaFiles moduledir
    l <- Lua.newstate
    Lua.openlibs l
    luafuncs <- fmap concat $ mapM (loadLuaFile l) funcfiles
    print luafuncs
    o <- Lua.callfunc l "create_static_sites" (Map.fromList [("a", "b"), ("c", "d")]) "x"
    -- putStrLn o
    print (o :: Map.Map String String)

--    Lua.callfunc l "create_static_sites" "test" "test" >>= putStrLn
    Lua.close l
