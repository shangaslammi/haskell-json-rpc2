{-# LANGUAGE OverloadedStrings #-}

module Network.JsonRpc2.Response where

import Network.JsonRpc2.Version
import Network.JsonRpc2.Error

import Control.Applicative
import Data.Aeson (parseJSON, FromJSON, ToJSON, Value(..), (.:), object, (.=))
import qualified Data.Aeson as Json
import qualified Data.HashMap.Strict as H

data Response
    = Success Json.Value Json.Value
    | Failure Error Json.Value

instance FromJSON Response where
    parseJSON (Object obj)
        | "result" `H.member` obj = parseSuccess
        | "error"  `H.member` obj = parseError
        | otherwise = fail "response must have either result or error"
        where
            parseSuccess = Success <$> obj .: "result" <*> parseId
            parseError   = Failure <$> obj .: "error"  <*> parseId
            parseId      = obj .: "id"

instance ToJSON Response where
    toJSON (Success result rqid) = object
        [ "result" .= result
        , "id" .= rqid
        ]

    toJSON (Failure err rqid) = object
        [ "error" .= err
        , "id" .= rqid
        ]


