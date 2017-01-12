{-# LANGUAGE OverloadedStrings, DeriveGeneric, GeneralizedNewtypeDeriving #-}

module Main where

import App
import Location
import Configuration
import Weather
import Weather.WeatherUnderground

import System.Console.ArgParser
import System.Exit
import Control.Exception.Safe
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Identity
import Data.Text (Text)
import qualified Data.Text as T


main :: IO ()
main = do
  interface <- getCmdLnInterface
  runApp interface run

run :: ParsedArgs ->  IO ()
run args = handle
            (\e -> die $ "Exited with: " ++ show (e :: SomeWeatherException))
            (do
              config <- getConfig
              runWithConfig getWeather config
              return ())


getWeather :: WeatherAppIO (SpecificConfig WeatherUndergroundApiKey) ()
getWeather = getLocation >>= getConditions >>= displayWeather

type ReportType = String
data ParsedArgs = ParsedArgs ReportType

myWeatherParser :: ParserSpec ParsedArgs
myWeatherParser = ParsedArgs
  `parsedBy` optPos "current" "reportType" `Descr` "what specifically about the weather do you want to know?"

getCmdLnInterface :: IO (CmdLnInterface ParsedArgs)
getCmdLnInterface = pure $ mkDefaultApp myWeatherParser "weather"