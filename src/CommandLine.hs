module CommandLine (
  ParsedArgs (..),
  getCmdLnInterface
) where

import System.Console.ArgParser
import System.Console.ArgParser.Params

data ParsedArgs = ParsedArgs { reportType :: String
                             , city :: String
                             , state :: String
                             , configure :: Bool
                             , debug :: Bool
                             } deriving (Show)

myWeatherParser :: ParserSpec ParsedArgs
myWeatherParser = ParsedArgs
  `parsedBy` optPos "current" "reportType" `Descr` "[current|hourly|daily] Defaults to 'current'"
  `andBy` optFlag "" "city" `Descr` "optionally specify a location (must be used with '--state')"
  `andBy` optFlag "" "state" `Descr` "optionally specify a location (must be used with '--city')"
  `andBy` FlagParam Long "configure" id `Descr` "add/modify an api key"
  `andBy` boolFlag "debug" `Descr` "print debugging information (for developers only)"

getCmdLnInterface :: IO (CmdLnInterface ParsedArgs)
getCmdLnInterface = pure $ mkDefaultApp myWeatherParser "weather"