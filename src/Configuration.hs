{-# LANGUAGE OverloadedStrings, DeriveGeneric, GeneralizedNewtypeDeriving #-}

module Configuration (
  WeatherUndergroundApiKey (..),
  WeatherBackendConfig,
  Config (..),
  SpecificConfig (..),
  ConfigException (..),
  getConfig,
  selectConfig
) where

import App

import System.IO

import Control.Exception.Safe
import Data.Typeable
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

data SpecificConfig a = SpecificConfig { weatherConfig :: a } deriving (Show)

data ConfigException = ConfigNotFound | ConfigParseException Text deriving (Show, Typeable)
instance Exception ConfigException where
  toException = weatherExceptionToException
  fromException = weatherExceptionFromException

configFilePath :: FilePath
configFilePath = "/Users/Sam/.weather"

selectConfig :: (Monad m, MonadThrow m)=> Config -> m (SpecificConfig WeatherUndergroundApiKey)
selectConfig (Config (WeatherConfigs (Just weatherUndergroundKey))) = return (SpecificConfig weatherUndergroundKey)
selectConfig (Config (WeatherConfigs Nothing)) = throwM ConfigNotFound


getConfig :: IO (SpecificConfig WeatherUndergroundApiKey)
getConfig = do
  configContents <- C.readFile configFilePath
  case decode' configContents >>= selectConfig of Nothing -> throwM $ ConfigParseException $ pack $ C.unpack configContents
                                                  Just config -> return config

