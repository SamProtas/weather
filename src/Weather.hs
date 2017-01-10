module Weather (
  CurrentConditions (..),
) where

import Configuration

import Data.Text


class CurrentConditions a where
  getCurrentConditions :: Config -> IO a
  showConditions :: a -> Text
