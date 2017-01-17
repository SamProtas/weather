{-# LANGUAGE OverloadedStrings, DeriveGeneric, GeneralizedNewtypeDeriving, DuplicateRecordFields, ScopedTypeVariables #-}

module Main where

import App
import CommandLine
import Location
import Configuration
import Weather
import Weather.WeatherUnderground

import Data.Char
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
run ParsedArgs { configure = True } = liftIO setWeatherUndergroundConfig
run args@ParsedArgs { debug = debug} = do
    config <- getConfig
    (_, log) <- runWithConfig (dispatchWeather args) config
    when debug $ sequence_ $ putStrLn <$> log -- TODO: Catch log rethrow on error. MonadCatch?
    return ()
    `catches`
    [ Handler (\(e :: ConfigException) -> putStrLn (toMessage e))
    , Handler (\(e :: SomeWeatherException) -> die $ "Exited with: " ++ show e) ]


dispatchWeather :: ParsedArgs -> WeatherAppIO (SpecificConfig WeatherUndergroundApiKey) ()
dispatchWeather args = getLocation >>= dispatchWeatherForLocation args
-- dispatchWeather args@ParsedArgs { city = "", state = ""} = getLocation >>= dispatchWeatherForLocation args
-- dispatchWeather ParsedArgs { city = city, state = state } = undefined -- TODO build location object here, parseLocation function?

-- TODO - use typeclass definitions for different backend support
dispatchWeatherForLocation :: ParsedArgs -> Location -> WeatherAppIO (SpecificConfig WeatherUndergroundApiKey) ()
dispatchWeatherForLocation ParsedArgs { reportType = reportType } location = getReport
  where getReport = case reportType of Current -> getConditions location >>= displayWeather
                                       Hourly -> getHourly location >>= displayWeather
                                       Daily -> getDaily location >>= displayWeather