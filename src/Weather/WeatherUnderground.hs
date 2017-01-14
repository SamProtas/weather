{-# LANGUAGE OverloadedStrings, DeriveGeneric, GeneralizedNewtypeDeriving #-}

module Weather.WeatherUnderground (
  getConditions,
  getHourly
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
import Data.List
import qualified Data.ByteString.Lazy.Char8 as C
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Simple
import Control.Exception.Safe
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Aeson
import Data.Aeson.Types
import Data.Bifunctor
import Data.Either
import qualified Data.HashMap.Lazy as Map


data Feature = Conditions | Hourly deriving (Show) -- Additional features added here
newtype WeatherUrl = WeatherUrl Text deriving (Show, Generic, IsString)

data CurrentObservationResponse = CurrentObservationResponse { current_observation :: CurrentObservation }
                                                             deriving (Show, Generic)
instance FromJSON CurrentObservationResponse
instance ToJSON CurrentObservationResponse

data CurrentObservation = CurrentObservation { observation_time :: String
                                             , weather :: String
                                             , temp_f :: Float
                                             , feelslike_f :: String
                                             , relative_humidity :: String
                                             , wind_string :: String
                                             , observation_location :: CurrentObservationLocation}
                                             deriving (Show, Generic)
instance FromJSON CurrentObservation
instance ToJSON CurrentObservation

data CurrentObservationLocation = CurrentObservationLocation { full :: String
                                                             , latitude :: String
                                                             , longitude :: String }
                                                             deriving (Show, Generic)
instance FromJSON CurrentObservationLocation
instance ToJSON CurrentObservationLocation


data HourForcast = HourForcast { prettyTime :: String
                               , tempFString :: String
                               , feelsLikeFString :: String
                               , condition :: String }
                               deriving (Show, Generic)
instance ToJSON HourForcast
instance FromJSON HourForcast where
  parseJSON (Object o) = do
    prettyTime <- o .: "FCTTIME" >>= parseObj "pretty"
    tempFString <- o .: "temp" >>= parseObj "english"
    feelslikeFString <- o .: "feelslike" >>= parseObj "english"
    condition <- o .: "condition"
    return $ HourForcast prettyTime tempFString feelslikeFString condition
      where parseObj :: Text -> Value -> Parser String
            parseObj key (Object o) = o .: key
            parseObj _ obj = typeMismatch "HourForcast" obj

data HourlyForcastResponse = HourlyForcastResponse { hourly_forecast :: [HourForcast] } deriving (Show, Generic)
instance ToJSON HourlyForcastResponse
instance FromJSON HourlyForcastResponse

lower :: Feature -> Text
lower feature = T.pack $ fmap toLower (show feature)

base = "http://api.wunderground.com/api/"

getUrl :: (FromJSON a, Show a) => Feature -> Location -> WeatherAppIO (SpecificConfig WeatherUndergroundApiKey) a
getUrl feature location = do
    config <- ask
    manager <- liftIO $ newManager tlsManagerSettings
    let url = buildUrl config feature location
    tell [show url]
    preRequest <- toRequest url
    let request = setRequestManager manager preRequest
    tell [show request]
    response <- httpJSON request
    tell [show response]
    let body = getResponseBody response
    return body

getConditions :: Location -> WeatherAppIO (SpecificConfig WeatherUndergroundApiKey) CurrentObservationResponse
getConditions = getUrl Conditions

getHourly :: Location -> WeatherAppIO (SpecificConfig WeatherUndergroundApiKey) HourlyForcastResponse
getHourly = getUrl Hourly

buildUrl :: SpecificConfig WeatherUndergroundApiKey -> Feature -> Location -> WeatherUrl
buildUrl (SpecificConfig (WeatherUndergroundApiKey apiKey)) feature (Location _ _ _ lat lon) =
  WeatherUrl $ T.concat [base, apiKey, "/", lower feature, "/q/", T.pack $ show lat, ",", T.pack $ show lon, ".json"]

toRequest :: MonadThrow m => WeatherUrl -> m Request
toRequest (WeatherUrl url) = parseRequest $ T.unpack url

degreesF = " °F"

instance DisplayAbleWeather CurrentObservationResponse where
  displayWeather obs@(CurrentObservationResponse (CurrentObservation
    time weatherDesc temp_f feelslike_f relative_humidity wind_string
    CurrentObservationLocation { full = locationString }
    )) = do
      tell [show obs]
      putM $ "Current Weather Report for: " ++ locationString
      putM weatherDesc
      putM $ "Temperature: " ++ show temp_f ++ degreesF
      putM $ "Feels like " ++ feelslike_f ++ degreesF
      putM $ "Wind: " ++ wind_string
      putM time


instance DisplayAbleWeather HourlyForcastResponse where
  displayWeather (HourlyForcastResponse hours) = sequence_ $ putM <$> formatHours hours

formatHours :: [HourForcast] -> [String]
formatHours = intercalate ["-------"] . fmap formatHourForcast

formatHourForcast :: HourForcast -> [String]
formatHourForcast (HourForcast time temp feels condition) =
  [ time
  , "Condtions: " ++ condition
  , "Temperature: " ++ temp ++ degreesF
  , "Feels Like: " ++ feels ++ degreesF ]