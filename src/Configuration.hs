{-# LANGUAGE OverloadedStrings, DeriveGeneric, GeneralizedNewtypeDeriving #-}

module Configuration (
  WeatherUndergroundApiKey (..),
  WeatherBackendConfig,
  Config (..),
  SpecificConfig (..),
  getConfig,
  selectConfig
) where

import System.IO

import Control.Exception
import Data.Text
import Data.String (IsString)
import Data.Bifunctor
import qualified Data.Text.IO as TIO
import qualified Data.ByteString.Lazy.Char8 as C
import GHC.Generics
import Data.Aeson


data Config = Config { weatherConfigs :: WeatherConfigs } deriving (Show, Generic)
instance ToJSON Config
instance FromJSON Config

data WeatherConfigs = WeatherConfigs { weatherUnderGround :: Maybe WeatherUndergroundApiKey } deriving (Show, Generic)
instance ToJSON WeatherConfigs
instance FromJSON WeatherConfigs

class (FromJSON a, Show a) => WeatherBackendConfig a

newtype WeatherUndergroundApiKey = WeatherUndergroundApiKey Text deriving (Show, Generic, IsString)
instance ToJSON WeatherUndergroundApiKey
instance FromJSON WeatherUndergroundApiKey
instance WeatherBackendConfig WeatherUndergroundApiKey


data SpecificConfig a = SpecificConfig { weather :: a } deriving (Show)


configFilePath :: FilePath
configFilePath = "/Users/Sam/.weather"

getConfig :: IO (Either Text (SpecificConfig WeatherUndergroundApiKey))
getConfig = do
  -- TODO - Add debugger that collects errors?
  configContents <- try $ C.readFile configFilePath  :: IO (Either IOError C.ByteString)
  let result = do
          contents <- first (\_ -> pack $ "Error accessing config located at: " ++ configFilePath) configContents -- :: Either Text C.ByteString
          config <- first (\_ -> pack $ "Error parsing config:\n" ++ show contents) $ eitherDecode' contents -- :: Either Text Config
          selected <- selectConfig config
          return selected

  return result

selectConfig :: Config -> Either Text (SpecificConfig WeatherUndergroundApiKey)
selectConfig (Config (WeatherConfigs (Just weatherUndergroundKey))) = Right (SpecificConfig weatherUndergroundKey)
selectConfig (Config (WeatherConfigs Nothing)) = Left "No weather backend included in configuration file"
