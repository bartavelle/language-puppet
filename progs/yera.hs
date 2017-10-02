module Main (main) where
  
import           Options.Applicative
import           Data.Monoid
import qualified Data.Text as T
import qualified Data.Either.Strict as S
import qualified Data.HashMap.Strict as HM
import           Text.PrettyPrint.ANSI.Leijen (pretty)

import Puppet.Interpreter.Types
import Puppet.Interpreter.PrettyPrinter()
import Hiera.Server

data Config
  = Config
  { _filepath    :: FilePath
  , _query       :: String
  , _queryType   :: HieraQueryType
  , _variables   :: [(T.Text,T.Text)]
  }

readQT :: String -> Maybe HieraQueryType
readQT s
  = case s of
    "first"  -> Just QFirst
    "unique" -> Just QUnique
    "hash"   -> Just QHash
    _        -> Nothing

parseVariable :: String -> Either String (T.Text, T.Text)
parseVariable s =
  case break (=='=') s of
    ([], []) -> Left "Empty variable"
    ([], _) -> Left "Nothing on the left side of the = symbol"
    (_, []) -> Left "Nothing on the right side of the = symbol"
    (var, '=':val) -> Right (T.pack var, T.pack val)
    _ -> Left "???"

configParser :: Parser Config
configParser = Config <$> strOption (long "config" <> short 'c' <> metavar "CONFIG" <> value "hiera.yaml")
                      <*> strOption (long "query" <> short 'q' <> metavar "QUERY")
                      <*> option (maybeReader readQT) (long "querytype" <> short 't' <> metavar "QUERYTYPE" <> value QFirst <> help "values: first (default), unique, hash")
                      <*> many (argument (eitherReader parseVariable) (metavar "VARIABLE" <> help "Variables, in the form key=value"))

configInfo :: ParserInfo Config
configInfo = info (configParser <**> helper) mempty

main :: IO ()
main = do
  Config fp query qtype vars <- execParser configInfo
  ehiera <- startHiera fp
  case ehiera of
    Left rr -> error rr
    Right hiera -> do
      r <- hiera (HM.fromList vars) (T.pack query) qtype
      case r of
        S.Left rr -> error (show rr)
        S.Right Nothing -> putStrLn "no match"
        S.Right (Just res) -> print (pretty res)