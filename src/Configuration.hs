{-# LANGUAGE OverloadedStrings, DeriveGeneric, GeneralizedNewtypeDeriving #-}

module Configuration (
  WeatherUndergroundApiKey (..),
  WeatherBackendConfig,
  Config (..),
  SpecificConfig (..),
  ConfigException (..),
  getConfig,
  selectConfig,
  setWeatherUndergroundConfig,
  toMessage
) where

import App

import System.IO
import System.IO.Error
import System.Environment

import Control.Exception.Safe
import Control.Monad.Catch (handleIf)
import Data.Typeable
import Data.String (IsString)
import Data.Bifunctor
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.ByteString.Lazy.Char8 as C
import GHC.Generics
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)


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

configFileName :: FilePath
configFileName = ".weather"

getConfigFilePath :: IO FilePath
getConfigFilePath = getEnv "HOME" >>= (\homeDir -> return $ homeDir ++ "/" ++ configFileName)

selectConfig :: (Monad m, MonadThrow m)=> Config -> m (SpecificConfig WeatherUndergroundApiKey)
selectConfig (Config (WeatherConfigs (Just weatherUndergroundKey))) = return (SpecificConfig weatherUndergroundKey)
selectConfig (Config (WeatherConfigs Nothing)) = throwM ConfigNotFound


getConfig :: IO (SpecificConfig WeatherUndergroundApiKey)
getConfig = handleIf isDoesNotExistError (\_ -> throw ConfigNotFound) $ do
  contents <- getConfigFilePath >>= C.readFile
  case decode' contents >>= selectConfig of Nothing -> throwM $ ConfigParseException $ T.pack $ C.unpack contents
                                            Just config -> return config

setWeatherUndergroundConfig :: IO ()
setWeatherUndergroundConfig = do
  putStrLn "You need a valid WeatherUnderGround API key to use this application."
  putStrLn "You can obtain an api key for FREE here: https://www.wunderground.com/member/registration?mode=api_signup"
  putStrLn "API KEY: "
  key <- getLine
  let config = encodePretty Config { weatherConfigs = WeatherConfigs { weatherUnderGround = Just $ WeatherUndergroundApiKey $ T.pack key }}
  configFilePath <- getConfigFilePath
  C.writeFile configFilePath config
  putStrLn ""
  putStrLn "Your API key has been set. Test it out with the command `weather`."


toMessage :: ConfigException -> String
toMessage ConfigNotFound = "Could not find an API key specified. " ++ runMessage
toMessage (ConfigParseException _) = "Could not parse the configuration file. " ++ runMessage

runMessage = "Run with command `weather --configure` to fix this issue."