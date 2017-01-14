{-# LANGUAGE OverloadedStrings, DeriveGeneric, GeneralizedNewtypeDeriving, DuplicateRecordFields #-}

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
run args@ParsedArgs { reportType = reportType,  debug = debug} = handle
  (\e -> die $ "Exited with: " ++ show (e :: SomeWeatherException))
  (do
    config <- getConfig
    (_, log) <- runWithConfig (dispatchArgs args) config
    when debug $ sequence_ $ putStrLn <$> log -- TODO: Catch log rethrow on error. MonadCatch?
    return ())


dispatchArgs :: ParsedArgs -> WeatherAppIO (SpecificConfig WeatherUndergroundApiKey) ()
dispatchArgs ParsedArgs { configure = True } = liftIO $ putStrLn "Configureing..." -- TODO: implement
dispatchArgs args = dispatchWeather args

dispatchWeather :: ParsedArgs -> WeatherAppIO (SpecificConfig WeatherUndergroundApiKey) ()
dispatchWeather args@ParsedArgs { city = "", state = ""} = getLocation >>= dispatchWeatherForLocation args
dispatchWeather ParsedArgs { city = city, state = state } = undefined -- TODO build location object here, parseLocation function?

-- TODO - use typeclass definitions for different backend support
dispatchWeatherForLocation :: ParsedArgs -> Location -> WeatherAppIO (SpecificConfig WeatherUndergroundApiKey) ()
dispatchWeatherForLocation ParsedArgs { reportType = reportType } location = getReport
  where getReport = case reportType of Current -> getConditions location >>= displayWeather
                                       Hourly -> getHourly location >>= displayWeather
                                       Daily -> undefined -- TODO: implement