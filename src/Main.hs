{-# LANGUAGE OverloadedStrings #-}

module Main where

import Location
import Configuration
import Weather.WeatherUnderground

import Data.Text (Text)
import qualified Data.Text as T

import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Simple

main :: IO ()
main = do
  eitherConfig <- getConfig :: IO (Either Text (SpecificConfig WeatherUndergroundApiKey))
  case eitherConfig of Left err -> sequence_ $ print <$> T.lines err
                       Right config -> do
                        eitherLocation <- getLocation
                        case eitherLocation of Left message -> print message
                                               Right location -> do
                                                res <- getConditions config location
                                                print res
                                                return ()
