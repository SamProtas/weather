{-# LANGUAGE OverloadedStrings #-}

module Main where

import Location

import Data.Text

main :: IO ()
main = do
  eitherLocation <- getLocation
  print $ case eitherLocation of Left message -> message
                                 Right location -> pack $ show location
