{-# LANGUAGE OverloadedStrings, DeriveGeneric, GeneralizedNewtypeDeriving #-}

module App (
  WeatherAppIO (..),
  runWithConfig
) where

import Data.Text
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Identity
import Control.Exception.Safe

newtype WeatherAppIO c a = WeatherAppIO {
  runWeatherIO :: ReaderT c (WriterT [Text] IO) a
  } deriving (Monad, Applicative, Functor, MonadReader c, MonadWriter [Text], MonadIO, MonadThrow)

runWithConfig :: WeatherAppIO c a -> c -> IO a
runWithConfig weather config = do
  (out, _) <- runWriterT (runReaderT (runWeatherIO weather) config)
  return out