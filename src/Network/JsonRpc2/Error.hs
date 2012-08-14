{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.JsonRpc2.Error where

import Network.JsonRpc2.Utils

import qualified Data.Aeson as Json
import Data.Aeson.TH
import Data.Text

parseErrorCode     = -32700 :: Int
invalidRequestCode = -32600 :: Int
methodNotFoundCode = -32601 :: Int
invalidParamsCode  = -32602 :: Int
internalErrorCode  = -32603 :: Int

data Error = Error
    { errCode    :: Int
    , errMessage :: Text
    , errData    :: Maybe Json.Value
    }

deriveJSON (mkJsonFieldName 3) ''Error

parseError      = Error parseErrorCode "error when parsing json" Nothing
invalidRequest  = Error invalidRequestCode "received json was not a valid request object" Nothing
invalidParams   = Error invalidParamsCode "invalid parameters" Nothing
methodNotFound  = Error methodNotFoundCode "unknown method" Nothing
internalError   = flip (Error internalErrorCode) Nothing
wrongResultType = internalError "the server returned an unexpected result type"