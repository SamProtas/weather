{-# LANGUAGE OverloadedStrings, DeriveGeneric, GeneralizedNewtypeDeriving #-}

module Main where

import App
import Location
import Configuration
import Weather
import Weather.WeatherUnderground

import System.Exit
import Control.Exception.Safe
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Identity
import Data.Text (Text)
import qualified Data.Text as T


main :: IO ()
main = handle
  (\e -> die $ "Exited with: " ++ show (e :: SomeWeatherException))
  (do
    config <- getConfig
    runWithConfig getWeather config
    return ())

getWeather :: WeatherAppIO (SpecificConfig WeatherUndergroundApiKey) ()
getWeather = getLocation >>= getConditions >>= displayWeather