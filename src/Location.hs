{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Location (
  getLocation,
  Location (..)
) where

import Data.Text
import GHC.Generics
import Data.Aeson
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Simple

data Location = Location { city :: Text
                         , region :: Text
                         , country :: Text }
                         deriving (Show, Generic)

instance FromJSON Location
instance ToJSON Location

locationUrl = "https://ipinfo.io/json"

getLocation :: IO (Either Text Location)
getLocation = do
  manager <- newManager tlsManagerSettings
  let request = setRequestManager manager locationUrl
  response <- httpJSONEither request
  let body = getResponseBody response
  return $ case body of Left _ -> Left "Error parsing your location"
                        Right location -> Right location
