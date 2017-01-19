module CommandLine (
  ParsedArgs (..),
  ReportType (..),
  getCmdLnInterface
) where

import Location

import Text.Read
import Data.Char
import Data.List
import System.Console.ArgParser
import System.Console.ArgParser.Params

data ParsedArgs = ParsedArgs { reportType :: ReportType
                             , city :: String
                             , state :: Maybe State
                             , country :: String
                             , configure :: Bool
                             , debug :: Bool
                             } deriving (Show)

stateParam :: StdArgParam (Maybe State)
stateParam = StdArgParam (Optional Nothing) Flag "state" (SingleArgParser stateParser)

stateParser :: Arg -> ParseResult (Maybe State)
stateParser s = Just <$> readEither normalized
  where normalized = fmap toUpper s


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

-- TODO: differentiate or turn off one of short flag for city/country (both -c)
myWeatherParser :: ParserSpec ParsedArgs
myWeatherParser = ParsedArgs
  `parsedBy` reportTypeParam `Descr` "[" ++ reportTypesPretty ++ "] defaults to 'current'"
  `andBy` optFlag "" "city" `Descr` "optionally specify a location (must be used with '--state' or '--country')"
  `andBy` stateParam `Descr` "optionally specify a location (must be used with '--city') ex: PA for Pennsylvania"
  `andBy` optFlag "us" "country" `Descr` "optionally specify a location (must be used with '--city') defaults to 'us'"
  `andBy` FlagParam Long "configure" id `Descr` "add/modify an api key"
  `andBy` boolFlag "debug" `Descr` "print debugging information (for developers only)"

getCmdLnInterface :: IO (CmdLnInterface ParsedArgs)
getCmdLnInterface = pure $ mkDefaultApp myWeatherParser "weather"

reportTypesPretty :: String
reportTypesPretty = intercalate "|" $ fmap toLower . show <$> enumFrom Current