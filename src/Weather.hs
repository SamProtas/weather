module Weather (
  CurrentConditions (..),
  DisplayAbleWeather (..),
  putM
) where

import App
import Configuration

import Data.Text
import Control.Monad
import Control.Monad.Reader
import Control.Exception.Safe


class CurrentConditions a where
  getCurrentConditions :: Config -> IO a
  showConditions :: a -> Text


class (Show a) => DisplayAbleWeather a where
  displayWeather :: a -> WeatherAppIO c ()
  displayWeather = liftIO . print

putM :: String -> WeatherAppIO c ()
putM = liftIO . putStrLn