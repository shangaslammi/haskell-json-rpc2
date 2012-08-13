{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.JsonRpc2.Server.Wai where

import Network.Wai
import Network.HTTP.Types
import Data.Conduit
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Operational

import qualified Data.Conduit.List as CL
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Network.JsonRpc2.Server
import Network.JsonRpc2.ByteConnection

byteConnSink :: Monad m => ByteConnection m a -> Sink B.ByteString m BL.ByteString
byteConnSink = lift . viewT >=> eval id where
    eval dl (GetSomeBytes :>>= cont) = do
            h <- CL.head
            lift . viewT . cont >=> eval dl $ case h of
                Nothing -> B.empty
                Just bs -> bs
    eval dl (WriteBytes bs :>>= cont) =
        lift . viewT . cont >=> eval ((bs:) . dl) $ ()
    eval dl _ = return $ BL.fromChunks . dl $ []


rpcWaiApp :: RpcServer (ResourceT IO) a -> Application
rpcWaiApp rpc =
    let byteConn = runByteConnectionServer rpc
        sink = byteConnSink byteConn
        headers = [("Content-Type", "application/json")]
    in \req -> fmap (responseLBS status200 headers) $ requestBody req $$ sink

