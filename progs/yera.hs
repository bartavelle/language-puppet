{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StrictData        #-}
module Main (main) where

import           Puppet.Prelude                   hiding (option)

import qualified Data.Either.Strict               as S
import qualified Data.HashMap.Strict              as HM
import qualified Data.List                        as List
import           Options.Applicative
import           Text.PrettyPrint.ANSI.Leijen     (pretty)

import           Hiera.Server
import           Puppet.Interpreter.PrettyPrinter ()
import           Puppet.Interpreter.Types

data Config
  = Config
  { _filepath  :: FilePath
  , _query     :: String
  , _queryType :: HieraQueryType
  , _variables :: [(Text,Text)]
  }

readQType :: String -> Maybe HieraQueryType
readQType s
  = case s of
    "first"  -> Just QFirst
    "unique" -> Just QUnique
    "hash"   -> Just QHash
    _        -> Nothing

parseVariable :: String -> Either String (Text, Text)
parseVariable s =
  case List.break (=='=') s of
    ([], [])       -> Left "Empty variable"
    ([], _)        -> Left "Nothing on the left side of the = symbol"
    (_, [])        -> Left "Nothing on the right side of the = symbol"
    (var, '=':val) -> Right (toS var, toS val)
    _              -> Left "???"

configParser :: Parser Config
configParser = Config <$> strOption (long "config" <> short 'c' <> metavar "CONFIG" <> value "hiera.yaml")
                      <*> strOption (long "query" <> short 'q' <> metavar "QUERY")
                      <*> option (maybeReader readQType) (long "querytype" <> short 't' <> metavar "QUERYTYPE" <> value QFirst <> help "values: first (default), unique, hash")
                      <*> many (argument (eitherReader parseVariable) (metavar "VARIABLE" <> help "Variables, in the form key=value"))

configInfo :: ParserInfo Config
configInfo = info (configParser <**> helper) mempty

main :: IO ()
main = do
  Config fp query qtype vars <- execParser configInfo
  hiera <- startHiera fp
  hiera (HM.fromList vars) (toS query) qtype >>= \case
    S.Left rr -> panic (show rr)
    S.Right Nothing -> die "no match"
    S.Right (Just res) -> print (pretty res)
