{-# LANGUAGE OverloadedStrings #-}

module Network.JsonRpc2.Version where

import Data.Aeson (Value)

versionString :: Value
versionString = "2.0"
