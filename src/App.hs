{-# LANGUAGE OverloadedStrings, DeriveGeneric, GeneralizedNewtypeDeriving, ExistentialQuantification #-}

module App (
  WeatherAppIO (..),
  runWithConfig,
  SomeWeatherException (..),
  weatherExceptionToException,
  weatherExceptionFromException
) where

import Data.Text
import Data.Typeable
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Identity
import Control.Exception.Safe

newtype WeatherAppIO c a = WeatherAppIO {
  runWeatherIO :: ReaderT c (WriterT [String] IO) a
  } deriving (Monad, Applicative, Functor, MonadReader c, MonadWriter [String], MonadIO, MonadThrow)

runWithConfig :: WeatherAppIO c a -> c -> IO (a, [String])
runWithConfig weather config =
  runWriterT (runReaderT (runWeatherIO weather) config)

data SomeWeatherException = forall e . Exception e => SomeWeatherException e deriving (Typeable)

instance Show SomeWeatherException where
  show (SomeWeatherException e) = show e

instance Exception SomeWeatherException

weatherExceptionToException :: Exception e => e -> SomeException
weatherExceptionToException = toException . SomeWeatherException

weatherExceptionFromException :: Exception e => SomeException -> Maybe e
weatherExceptionFromException x = do
  SomeWeatherException a <- fromException x
  cast a