{-# LANGUAGE TemplateHaskell #-}

module Facter where

import Control.Lens
import Data.Aeson
import Data.Char
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.List as List
import qualified Data.List.Split as List
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import Puppet.Language
import qualified System.Directory as Directory
import System.Environment
import System.Posix.Unistd (SystemID (..), getSystemID)
import System.Posix.User
import Text.Printf
import Prelude

type Facts = HM.HashMap T.Text PValue

data FactInfo = FactInfo
  { _factInfoNodename :: !NodeName,
    _factInfoName :: !T.Text,
    _factInfoVal :: !PValue
  }

makeClassy ''FactInfo

instance ToJSON FactInfo where
  toJSON (FactInfo n f v) = object [("certname", String n), ("name", String f), ("value", toJSON v)]

instance FromJSON FactInfo where
  parseJSON (Object v) = FactInfo <$> v .: "certname" <*> v .: "name" <*> v .: "value"
  parseJSON _ = fail "invalid fact info"

storageunits :: [(String, Int)]
storageunits = [("", 0), ("K", 1), ("M", 2), ("G", 3), ("T", 4)]

getPrefix :: Int -> String
getPrefix n
  | null fltr = error $ "Could not get unit prefix for order " <> show n
  | otherwise = fst $ head fltr
  where
    fltr = filter (\(_, x) -> x == n) storageunits

getOrder :: String -> Int
getOrder n
  | null fltr = error $ "Could not get order for unit prefix " <> show n
  | otherwise = snd $ head fltr
  where
    nu = map toUpper n
    fltr = filter (\(x, _) -> x == nu) storageunits

normalizeUnit :: (Double, Int) -> Double -> (Double, Int)
normalizeUnit (unit, order) base
  | unit > base = normalizeUnit (unit / base, order + 1) base
  | otherwise = (unit, order)

storagedesc :: (String, String) -> String
storagedesc (ssize, unit) =
  let size = read ssize :: Double
      uprefix
        | unit == "B" = ""
        | otherwise = [head unit]
      uorder = getOrder uprefix
      (osize, oorder) = normalizeUnit (size, uorder) 1024
   in printf "%.2f %sB" osize (getPrefix oorder)

factRAM :: IO [(String, String)]
factRAM = do
  meminfo <- fmap (map words . lines) (readFile "/proc/meminfo")
  let memtotal = ginfo "MemTotal:"
      memfree = ginfo "MemFree:"
      swapfree = ginfo "SwapFree:"
      swaptotal = ginfo "SwapTotal:"
      ginfo st = sdesc $ head $ filter ((== st) . head) meminfo
      sdesc [_, size, unit] = storagedesc (size, unit)
      sdesc _ = storagedesc ("1", "B")
  return [("memorysize", memtotal), ("memoryfree", memfree), ("swapfree", swapfree), ("swapsize", swaptotal)]

factNET :: IO [(String, String)]
factNET = return [("ipaddress", "192.168.0.1")]

factOS :: IO [(String, String)]
factOS = do
  islsb <- Directory.doesFileExist "/etc/lsb-release"
  isdeb <- Directory.doesFileExist "/etc/debian_version"
  case (islsb, isdeb) of
    (True, _) -> factOSLSB
    (_, True) -> factOSDebian
    _ -> return []

factOSDebian :: IO [(String, String)]
factOSDebian = fmap (toV . head . lines) (readFile "/etc/debian_version")
  where
    toV v =
      [ ("lsbdistid", "Debian"),
        ("operatingsystem", "Debian"),
        ("lsbdistrelease", v),
        ("operatingsystemrelease", v),
        ("lsbmajdistrelease", takeWhile (/= '.') v),
        ("osfamily", "Debian"),
        ("lsbdistcodename", codename v),
        ("lsbdistdescription", "Debian GNU/Linux " <> v <> " (" <> codename v <> ")")
      ]
    codename v
      | null v = "unknown"
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
      where
        h x = head v == x

factOSLSB :: IO [(String, String)]
factOSLSB = do
  lsb <- fmap (map (break (== '=')) . lines) (readFile "/etc/lsb-release")
  let getval st
        | null filterd = "?"
        | otherwise = rvalue
        where
          filterd = filter (\(k, _) -> k == st) lsb
          value = (tail . snd . head) filterd
          rvalue
            | head value == '"' = read value
            | otherwise = value
      lrelease = getval "DISTRIB_RELEASE"
      distid = getval "DISTRIB_ID"
      maj
        | lrelease == "?" = "?"
        | otherwise = takeWhile (/= '.') lrelease
      osfam
        | distid == "Ubuntu" = "Debian"
        | otherwise = distid
  return
    [ ("lsbdistid", distid),
      ("operatingsystem", distid),
      ("lsbdistrelease", lrelease),
      ("operatingsystemrelease", lrelease),
      ("operatingsystemmajrelease", lrelease),
      ("lsbmajdistrelease", maj),
      ("lsbminordistrelease", ""),
      ("osfamily", osfam),
      ("lsbdistcodename", getval "DISTRIB_CODENAME"),
      ("lsbdistdescription", getval "DISTRIB_DESCRIPTION")
    ]

factMountPoints :: IO [(String, String)]
factMountPoints = do
  mountinfo <- fmap (map words . lines) (readFile "/proc/mounts")
  let ignorefs =
        HS.fromList
          [ "NFS",
            "nfs",
            "nfs4",
            "nfsd",
            "afs",
            "binfmt_misc",
            "proc",
            "smbfs",
            "autofs",
            "iso9660",
            "ncpfs",
            "coda",
            "devpts",
            "ftpfs",
            "devfs",
            "mfs",
            "shfs",
            "sysfs",
            "cifs",
            "lustre_lite",
            "tmpfs",
            "usbfs",
            "udf",
            "fusectl",
            "fuse.snapshotfs",
            "rpc_pipefs",
            "configfs",
            "devtmpfs",
            "debugfs",
            "securityfs",
            "ecryptfs",
            "fuse.gvfs-fuse-daemon",
            "rootfs"
          ]
      goodlines = filter (\x -> not $ HS.member (x !! 2) ignorefs) mountinfo
      goodfs = map (!! 1) goodlines
  return [("mountpoints", unwords goodfs)]

fversion :: IO [(String, String)]
fversion = return [("facterversion", "0.1"), ("environment", "test")]

factUser :: IO [(String, String)]
factUser = do
  username <- getEffectiveUserName
  return [("id", username)]

factUName :: IO [(String, String)]
factUName = do
  SystemID sn nn rl _ mc <- getSystemID
  let vparts = List.splitOn "." (takeWhile (/= '-') rl)
  return
    [ ("kernel", sn), -- Linux
      ("kernelmajversion", List.intercalate "." (take 2 vparts)), -- 3.5
      ("kernelrelease", rl), -- 3.5.0-45-generic
      ("kernelversion", List.intercalate "." (take 3 vparts)), -- 3.5.0
      ("hardwareisa", mc), -- x86_64
      ("hardwaremodel", mc), -- x86_64
      ("hostname", nn)
    ]

fenv :: IO [(String, String)]
fenv = do
  path <- getEnv "PATH"
  return [("path", path)]

factProcessor :: IO [(String, String)]
factProcessor = do
  cpuinfo <- readFile "/proc/cpuinfo"
  let cpuinfos = zip ["processor" <> show (n :: Int) | n <- [0 ..]] modelnames
      modelnames = mapMaybe (fmap (dropWhile (`elem` ("\t :" :: String))) . List.stripPrefix "model name") (lines cpuinfo)
  return $ ("processorcount", show (length cpuinfos)) : cpuinfos
