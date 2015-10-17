{-# LANGUAGE LambdaCase #-}
module Facter where

import Data.Char
import Text.Printf
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import Puppet.Interpreter.Types
import qualified Data.Text as T
import Control.Arrow
import Control.Lens
import System.Posix.User
import System.Posix.Unistd (getSystemID, SystemID(..))
import Data.List.Split (splitOn)
import Data.List (intercalate,stripPrefix)
import System.Environment
import System.Directory (doesFileExist)
import Data.Maybe (mapMaybe)
import Control.Monad.Trans.Either

storageunits :: [(String, Int)]
storageunits = [ ("", 0), ("K", 1), ("M", 2), ("G", 3), ("T", 4) ]

getPrefix :: Int -> String
getPrefix n | null fltr = error $ "Could not get unit prefix for order " ++ show n
            | otherwise = fst $ head fltr
    where fltr = filter (\(_, x) -> x == n) storageunits

getOrder :: String -> Int
getOrder n | null fltr = error $ "Could not get order for unit prefix " ++ show n
           | otherwise = snd $ head fltr
    where
        nu = map toUpper n
        fltr = filter (\(x, _) -> x == nu) storageunits

normalizeUnit :: (Double, Int) -> Double -> (Double, Int)
normalizeUnit (unit, order) base | unit > base = normalizeUnit (unit/base, order + 1) base
                                 | otherwise = (unit, order)

storagedesc :: (String, String) -> String
storagedesc (ssize, unit) = let
    size = read ssize :: Double
    uprefix | unit == "B" = ""
            | otherwise = [head unit]
    uorder = getOrder uprefix
    (osize, oorder) = normalizeUnit (size, uorder) 1024
    in printf "%.2f %sB" osize (getPrefix oorder)

factRAM :: IO [(String, String)]
factRAM = do
    meminfo <- fmap (map words . lines) (readFile "/proc/meminfo")
    let memtotal  = ginfo "MemTotal:"
        memfree   = ginfo "MemFree:"
        swapfree  = ginfo "SwapFree:"
        swaptotal = ginfo "SwapTotal:"
        ginfo st  = sdesc $ head $ filter ((== st) . head) meminfo
        sdesc [_, size, unit] = storagedesc (size, unit)
        sdesc _ = storagedesc ("1","B")
    return [("memorysize", memtotal), ("memoryfree", memfree), ("swapfree", swapfree), ("swapsize", swaptotal)]

factNET :: IO [(String, String)]
factNET = return [("ipaddress", "192.168.0.1")]

factOS :: IO [(String, String)]
factOS = do
    islsb <- doesFileExist "/etc/lsb-release"
    isdeb <- doesFileExist "/etc/debian_version"
    case (islsb, isdeb) of
        (True, _) -> factOSLSB
        (_, True) -> factOSDebian
        _ -> return []

factOSDebian :: IO [(String, String)]
factOSDebian = fmap (toV . head . lines) (readFile "/etc/debian_version")
    where
        toV v = [ ("lsbdistid"              , "Debian")
                , ("operatingsystem"        , "Debian")
                , ("lsbdistrelease"         , v)
                , ("operatingsystemrelease" , v)
                , ("lsbmajdistrelease"      , takeWhile (/='.') v)
                , ("osfamily"               , "Debian")
                , ("lsbdistcodename"        , codename v)
                , ("lsbdistdescription"     , "Debian GNU/Linux " ++ v ++ " (" ++ codename v ++ ")")
                ]
        codename v | null v = "unknown"
                   | h '7' = "wheezy"
                   | h '6' = "squeeze"
                   | h '5' = "lenny"
                   | h '4' = "etch"
                   | v == "3.1" = "sarge"
                   | v == "3.0" = "woody"
                   | v == "2.2" = "potato"
                   | v == "2.1" = "slink"
                   | v == "2.0" = "hamm"
                   | otherwise = "unknown"
            where h x = head v == x

factOSLSB :: IO [(String, String)]
factOSLSB = do
    lsb <- fmap (map (break (== '=')) . lines) (readFile "/etc/lsb-release")
    let getval st | null filterd = "?"
                  | otherwise = rvalue
                  where filterd = filter (\(k,_) -> k == st) lsb
                        value    = (tail . snd . head) filterd
                        rvalue | head value == '"' = read value
                               | otherwise         = value
        lrelease = getval "DISTRIB_RELEASE"
        distid  = getval "DISTRIB_ID"
        maj     | lrelease == "?" = "?"
                | otherwise = takeWhile (/= '.') lrelease
        osfam   | distid == "Ubuntu" = "Debian"
                | otherwise = distid
    return  [ ("lsbdistid"              , distid)
            , ("operatingsystem"        , distid)
            , ("lsbdistrelease"         , lrelease)
            , ("operatingsystemrelease" , lrelease)
            , ("lsbmajdistrelease"      , maj)
            , ("osfamily"               , osfam)
            , ("lsbdistcodename"        , getval "DISTRIB_CODENAME")
            , ("lsbdistdescription"     , getval "DISTRIB_DESCRIPTION")
            ]

factMountPoints :: IO [(String, String)]
factMountPoints = do
    mountinfo <- fmap (map words . lines) (readFile "/proc/mounts")
    let ignorefs = HS.fromList
                    ["NFS", "nfs", "nfs4", "nfsd", "afs", "binfmt_misc", "proc", "smbfs",
                    "autofs", "iso9660", "ncpfs", "coda", "devpts", "ftpfs", "devfs",
                    "mfs", "shfs", "sysfs", "cifs", "lustre_lite", "tmpfs", "usbfs", "udf",
                    "fusectl", "fuse.snapshotfs", "rpc_pipefs", "configfs", "devtmpfs",
                    "debugfs", "securityfs", "ecryptfs", "fuse.gvfs-fuse-daemon", "rootfs"
                    ]
        goodlines = filter (\x -> not $ HS.member (x !! 2) ignorefs) mountinfo
        goodfs = map (!! 1) goodlines
    return [("mountpoints", unwords goodfs)]

fversion :: IO [(String, String)]
fversion = return [("facterversion", "0.1"),("environment","test")]

factUser :: IO [(String, String)]
factUser = do
    username <- getEffectiveUserName
    return [("id",username)]

factUName :: IO [(String, String)]
factUName = do
    SystemID sn nn rl _ mc <- getSystemID
    let vparts = splitOn "." (takeWhile (/='-') rl)
    return [ ("kernel"           , sn)                              -- Linux
           , ("kernelmajversion" , intercalate "." (take 2 vparts)) -- 3.5
           , ("kernelrelease"    , rl)                              -- 3.5.0-45-generic
           , ("kernelversion"    , intercalate "." (take 3 vparts)) -- 3.5.0
           , ("hardwareisa"      , mc)                              -- x86_64
           , ("hardwaremodel"    , mc)                              -- x86_64
           , ("hostname"         , nn)
           ]

fenv :: IO [(String,String)]
fenv = do
    path <- getEnv "PATH"
    return [ ("path", path) ]

factProcessor :: IO [(String,String)]
factProcessor = do
    cpuinfo <- readFile "/proc/cpuinfo"
    let cpuinfos = zip [ "processor" ++ show (n :: Int) | n <- [0..]] modelnames
        modelnames = mapMaybe (fmap (dropWhile (`elem` ("\t :" :: String))) . stripPrefix "model name") (lines cpuinfo)
    return $ ("processorcount", show (length cpuinfos)) : cpuinfos

puppetDBFacts :: Nodename -> PuppetDBAPI IO -> IO (Container PValue)
puppetDBFacts node pdbapi =
    runEitherT (getFacts pdbapi (QEqual FCertname node)) >>= \case
        Right facts@(_:_) -> return (HM.fromList (map (\f -> (f ^. factname, f ^. factval)) facts))
        _ -> do
            rawFacts <- fmap concat (sequence [factNET, factRAM, factOS, fversion, factMountPoints, factOS, factUser, factUName, fenv, factProcessor])
            let ofacts = genFacts $ map (T.pack *** T.pack) rawFacts
                (hostname, ddomainname) = T.break (== '.') node
                domainname = if T.null ddomainname
                                 then ""
                                 else T.tail ddomainname
                nfacts = genFacts [ ("fqdn", node)
                                  , ("hostname", hostname)
                                  , ("domain", domainname)
                                  , ("rootrsa", "xxx")
                                  , ("operatingsystem", "Ubuntu")
                                  , ("puppetversion", "language-puppet")
                                  , ("virtual", "xenu")
                                  , ("clientcert", node)
                                  , ("is_virtual", "true")
                                  , ("concat_basedir", "/var/lib/puppet/concat")
                                  ]
                allfacts = nfacts `HM.union` ofacts
                genFacts = HM.fromList
            return (allfacts & traverse %~ PString)
