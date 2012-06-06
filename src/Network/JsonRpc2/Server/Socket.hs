{-# LANGUAGE TypeFamilies #-}

module Network.JsonRpc2.Server.Socket where

import Network.JsonRpc2.Server
import Network.JsonRpc2.Response
import Network.JsonRpc2.Request
import Network.JsonRpc2.Error

import Control.Applicative
import Control.Monad
import Data.Aeson (json', Value(..), fromJSON, encode)
import qualified Data.Aeson as A
import Data.Attoparsec.ByteString (parseWith, IResult(..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Control.Concurrent
import Network
import System.IO

data SocketRpcServer = SocketRpcServer Socket

instance Server SocketRpcServer where
    type ServerM SocketRpcServer = IO

    serveOnceM (SocketRpcServer sock) func = do
        (h, host, port) <- accept sock
        hSetBinaryMode h True
        hSetBuffering h NoBuffering
        void $ forkIO $ serveClient h B.empty
        where
            serveClient h buf = do
                hIsReadable h >>= guard
                let respond = BL.hPut h . encode
                r <- parseWith (B.hGetSome h 1024) json' buf
                case r of
                    Done xtra js -> case fromJSON js of
                        A.Error _  -> do
                            respond $ Failure invalidRequest Null
                            hClose h
                        A.Success req -> do
                            case req of
                                Request _ _ Nothing -> void $ func req
                                _                   -> func req >>= respond
                            serveClient h xtra
                    _            -> do
                        respond $ Failure parseError Null
                        hClose h

