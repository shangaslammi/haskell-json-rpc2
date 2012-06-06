{-# LANGUAGE OverloadedStrings #-}

module Network.JsonRpc2.Request where

import Network.JsonRpc2.Version

import Control.Applicative
import Data.Text
import Data.Aeson

data Request = Request Text RequestParams (Maybe Value)

data RequestParams
    = ArrayParams  Array
    | ObjectParams Object

instance FromJSON Request where
    parseJSON (Object obj) = Request
        <$> obj .: "method"
        <*> obj .: "params"
        <*> obj .:? "id"
    parseJSON _ = fail "response must be an object"

instance ToJSON Request where
    toJSON (Request method params rqid) = object $
        [ "jsonrpc" .= versionString
        , "method"  .= method
        , "params"  .= params
        ] ++ case rqid of
            Nothing   -> []
            Just rqid -> ["id" .= rqid]

instance FromJSON RequestParams where
    parseJSON (Array arr)  = return $ ArrayParams arr
    parseJSON (Object obj) = return $ ObjectParams obj
    parseJSON _ = fail "parameters must have either array or object format"

instance ToJSON RequestParams where
    toJSON (ArrayParams arr)  = Array arr
    toJSON (ObjectParams obj) = Object obj
