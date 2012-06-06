{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Network.JsonRpc2.Server where

import Network.JsonRpc2.Request
import Network.JsonRpc2.Response
import Network.JsonRpc2.Error

import Control.Monad
import Control.Monad.IO.Class

import Data.Aeson (parseJSON, FromJSON, ToJSON, Value(..), (.:))

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B

class Monad (ServerM s) => Server s where
    type ServerM s :: * -> *

    serve :: s -> (Request -> Response) -> ServerM s ()
    serve s = forever . serveOnce s

    serveOnce :: s -> (Request -> Response) -> ServerM s ()
    serveOnce s f = serveOnceM s (return . f)

    serveM :: s -> (Request -> ServerM s Response) -> ServerM s ()
    serveM s = forever . serveOnceM s

    serveOnceM :: s -> (Request -> ServerM s Response) -> ServerM s ()
