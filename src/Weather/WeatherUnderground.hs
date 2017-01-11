{-# LANGUAGE OverloadedStrings, DeriveGeneric, GeneralizedNewtypeDeriving #-}

module Weather.WeatherUnderground (
  getConditions,
  Feature (..)
) where

import App
import Weather
import Configuration
import Location

import GHC.Generics
import Data.Text (Text)
import qualified Data.Text as T
import Data.String (IsString)
import Data.Char
import qualified Data.ByteString.Lazy.Char8 as C
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Simple
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
import Data.Bifunctor
import Data.Either


data Feature = Conditions deriving (Show) -- Additional features added here
newtype WeatherUrl = WeatherUrl Text deriving (Show, Generic, IsString)

data CurrentObservationResponse = CurrentObservationResponse { current_observation :: CurrentObservation }
                                                             deriving (Show, Generic)
instance FromJSON CurrentObservationResponse
instance ToJSON CurrentObservationResponse

data CurrentObservation = CurrentObservation { observation_time :: Text
                                             , weather :: Text
                                             , temp_f :: Float
                                             , feelslike_f :: Text
                                             , relative_humidity :: Text
                                             , wind_string :: Text
                                             , observation_location :: CurrentObservationLocation}
                                             deriving (Show, Generic)
instance FromJSON CurrentObservation
instance ToJSON CurrentObservation

data CurrentObservationLocation = CurrentObservationLocation { full :: Text
                                                             , latitude :: Text
                                                             , longitude :: Text }
                                                             deriving (Show, Generic)
instance FromJSON CurrentObservationLocation
instance ToJSON CurrentObservationLocation

lower :: Feature -> Text
lower feature = T.pack $ fmap toLower (show feature)

base = "http://api.wunderground.com/api/"

getUrl :: FromJSON a => Feature -> Location -> WeatherAppIO (SpecificConfig WeatherUndergroundApiKey) a
getUrl feature location = do
    config <- ask
    manager <- liftIO $ newManager tlsManagerSettings
    let url = buildUrl config feature location
    preRequest <- toRequest url
    let request = setRequestManager manager preRequest
    response <- httpJSON request
    let body = getResponseBody response
    return body

getConditions :: Location -> WeatherAppIO (SpecificConfig WeatherUndergroundApiKey) CurrentObservationResponse
getConditions = getUrl Conditions

buildUrl :: SpecificConfig WeatherUndergroundApiKey -> Feature -> Location -> WeatherUrl
buildUrl (SpecificConfig (WeatherUndergroundApiKey apiKey)) feature (Location _ _ _ lat lon) =
  WeatherUrl $ T.concat [base, apiKey, "/", lower feature, "/q/", T.pack $ show lat, ",", T.pack $ show lon, ".json"]

toRequest :: MonadThrow m => WeatherUrl -> m Request
toRequest (WeatherUrl url) = parseRequest $ T.unpack url
