{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StrictData        #-}
module Main (main) where

import           XPrelude            hiding (option)

import qualified Data.HashMap.Strict as Map
import qualified Data.List           as List
import           Options.Applicative

import           Hiera.Server
import           Puppet.Language (PValue(..))

data Config
  = Config
  { _filepath  :: FilePath
  , _query     :: String
  , _queryType :: HieraQueryType
  , _variables :: [(Text,Text)]
  }

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
                      <*> option (maybeReader (readQueryType.toS)) (long "querytype" <> short 't' <> metavar "QUERYTYPE" <> value QFirst <> help "values: first (default), unique, hash")
                      <*> many (argument (eitherReader parseVariable) (metavar "VARIABLE" <> help "Variables, in the form key=value"))

configInfo :: ParserInfo Config
configInfo = info (configParser <**> helper) mempty

main :: IO ()
main = do
  Config fp query qtype vars <- execParser configInfo
  hiera <- startHiera "yera" fp
  runExceptT (hiera (PString <$> Map.fromList vars) (toS query) qtype) >>= \case
    Left rr -> panic (show rr)
    Right Nothing -> die "no match"
    Right (Just res) -> print (pretty res)
