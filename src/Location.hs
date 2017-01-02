{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Location (
  getLocation,
  Location (..)
) where

import Text.Read
import Data.Text
import GHC.Generics
import Data.Aeson
import Data.Aeson.Types
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Simple

data Location = Location { city :: Text
                         , region :: Text
                         , country :: Text
                         , latitude :: Float
                         , longitude :: Float }
                         deriving (Show, Generic)

instance ToJSON Location
instance FromJSON Location where
  parseJSON (Object o) = do
    parsedCity <- o .: "city"
    parsedRegion <- o .: "region"
    parsedCountry <- o .: "country"
    location <- o .: "loc"
    (parsedLat, parsedLong) <- locToLatLong location
    return $ Location parsedCity parsedRegion parsedCountry parsedLat parsedLong
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

getLocation :: IO (Either Text Location)
getLocation = do
  manager <- newManager tlsManagerSettings
  let request = setRequestManager manager locationUrl
  response <- httpJSONEither request
  let body = getResponseBody response
  return $ case body of Left _ -> Left "Error parsing your location"
                        Right location -> Right location
