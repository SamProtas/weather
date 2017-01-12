{-# LANGUAGE OverloadedStrings, DeriveGeneric, GeneralizedNewtypeDeriving #-}

module Main where

import App
import Location
import Configuration
import Weather
import Weather.WeatherUnderground

import Control.Exception.Safe
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Identity
import Data.Text (Text)
import qualified Data.Text as T


main :: IO ()
main = do
  config <- getConfig `withException` (\e -> putStrLn $ "Exited with: " ++ show (e :: SomeWeatherException))
  runWithConfig weather config `catch` reportExceptions
  return ()

weather :: WeatherAppIO (SpecificConfig WeatherUndergroundApiKey) ()
weather = getLocation >>= getConditions >>= displayWeather


-- TODO - Generalize to other exception types or fix exception hierarchy
reportExceptions :: ConfigException -> IO ()
reportExceptions ConfigNotFound = print "Could not find configuation"
reportExceptions (ConfigParseException raw) = print "Could not parse configuration" >> print "" >> print raw