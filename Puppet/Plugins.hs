module Puppet.Plugins (initLua, puppetFunc, closeLua) where

import Prelude hiding (catch)
import qualified Scripting.Lua as Lua
import Scripting.LuaUtils()
import System.Directory
import Control.Exception
import Data.String.Utils (endswith)
import qualified Data.Map as Map
import Control.Monad
import Data.Maybe (fromJust)
import Control.Monad.Loops (whileM)
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
                                        then return $ Just (ResolvedArray (map snd cp))
                                        else return $ Just (ResolvedHash cp)
                        _ -> return Nothing
                _ -> return Nothing

        valuetype _ = Lua.TUSERDATA

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

puppetFunc :: Lua.LuaState -> String -> [ResolvedValue] -> CatalogMonad ResolvedValue
puppetFunc l fn args = do
    content <- liftIO $ catch (fmap Right (Lua.callfunc l fn args)) (\e -> return $ Left $ show (e :: IOException))
    case content of
        Right x -> return x
        Left  y -> throwPosError y

initLua :: String -> IO (Lua.LuaState, [String])
initLua moduledir = do
    funcfiles <- getLuaFiles moduledir
    l <- Lua.newstate
    Lua.openlibs l
    luafuncs <- fmap concat $ mapM (loadLuaFile l) funcfiles
    return (l , luafuncs)

closeLua :: Lua.LuaState -> IO ()
closeLua = Lua.close
