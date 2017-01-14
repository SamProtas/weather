module CommandLine (
  ParsedArgs (..),
  ReportType (..),
  getCmdLnInterface
) where

import Data.Char
import Data.List
import System.Console.ArgParser
import System.Console.ArgParser.Params

data ParsedArgs = ParsedArgs { reportType :: ReportType
--                              , city :: String
--                              , state :: String
--                              , configure :: Bool
                             , debug :: Bool
                             } deriving (Show)

data ReportType = Current
                | Hourly
                | Daily
                deriving (Show, Eq, Ord, Bounded, Enum)

reportTypeParam :: StdArgParam ReportType
reportTypeParam = StdArgParam (Optional Current) Pos "reportType" (SingleArgParser reportTypeParser)

reportTypeParser :: Arg -> ParseResult ReportType
reportTypeParser s
  | lowerReportType == "current" = Right Current
  | lowerReportType == "daily" = Right Daily
  | lowerReportType == "hourly" = Right Hourly
  | otherwise = Left $ s ++ " is not a valid weather report type. Use '-h' to see your options."
    where lowerReportType = fmap toLower s

myWeatherParser :: ParserSpec ParsedArgs
myWeatherParser = ParsedArgs
  `parsedBy` reportTypeParam `Descr` "[" ++ reportTypesPretty ++ "] Defaults to 'current'"
--   `andBy` optFlag "" "city" `Descr` "optionally specify a location (must be used with '--state')"
--   `andBy` optFlag "" "state" `Descr` "optionally specify a location (must be used with '--city')"
--   `andBy` FlagParam Long "configure" id `Descr` "add/modify an api key"
  `andBy` boolFlag "debug" `Descr` "print debugging information (for developers only)"

getCmdLnInterface :: IO (CmdLnInterface ParsedArgs)
getCmdLnInterface = pure $ mkDefaultApp myWeatherParser "weather"

reportTypesPretty :: String
reportTypesPretty = intercalate "|" $ fmap toLower . show <$> enumFrom Current