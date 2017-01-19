{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Location (
  getLocation,
  Location (..),
  State (..)
) where

import App

import Text.Read
import Data.Text
import GHC.Generics
import Data.Aeson
import Data.Aeson.Types
import Control.Monad.Except
import Control.Monad.Writer
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Simple

data Location = Location { city :: Maybe String
                         , region :: Maybe String
                         , country :: Maybe String
                         , latitude :: Maybe Float
                         , longitude :: Maybe Float }
                         deriving (Show, Generic)

instance ToJSON Location
instance FromJSON Location where
  parseJSON (Object o) = do
    parsedCity <- o .:? "city"
    parsedRegion <- o .:? "region"
    parsedCountry <- o .:? "country"
    location <- o .: "loc"
    (parsedLat, parsedLong) <- locToLatLong location
    return $ Location parsedCity parsedRegion parsedCountry (Just parsedLat) (Just parsedLong)
    where
      locToLatLong :: Text -> Parser (Float, Float)
      locToLatLong loc = parseLatLong $ split (==',') loc
      parseLatLong :: [Text] -> Parser (Float, Float)
      parseLatLong [textLat, textLong] = case (,) <$> readMaybe (unpack textLat) <*> readMaybe (unpack textLong)
          of Just parsed -> return parsed
             _ -> parseFailure
      parseLatLong _ = parseFailure
      parseFailure = fail "Could not parse latitude/longitude"


locationUrl = "https://ipinfo.io/json"

getLocation :: WeatherAppIO c Location
getLocation = do
   manager <- liftIO $ newManager tlsManagerSettings
   let request = setRequestManager manager locationUrl
   response <- httpJSON request
   tell [show response]
   let body = getResponseBody response
   return body

data State =
  AL | AK | AZ | AR | CA | CO | CT | DE | FL | GA | HI | ID | IL | IN | IA | KS | KY | LA | ME | MD | MA | MI | MN |
  MS | MO | MT | NE | NV | NH | NJ | NM | NY | NC | ND | OH | OK | OR | PA | RI | SC | SD | TN | TX | UT | VT | VA |
  WA | WV | WI | WY deriving (Show, Enum, Read)
