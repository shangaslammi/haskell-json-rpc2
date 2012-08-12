{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

module Network.JsonRpc2.Server.Socket where

import Network.JsonRpc2.Server
import Network.JsonRpc2.ByteConnection

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Operational

import Control.Concurrent
import System.IO
import Network

import qualified Data.ByteString as B


class SocketServerMonad m where
    toIO :: m a -> IO a

instance SocketServerMonad IO where
    toIO = id

runSocketByteConnection :: MonadIO m => Handle -> ByteConnection m a -> m a
runSocketByteConnection h = viewT >=> eval where
    eval (Return a) = return a
    eval (instr :>>= cont) = case instr of
        GetSomeBytes  -> liftIO (B.hGetSome h 1024)
        WriteBytes bs -> liftIO (B.hPut h bs)
        EndResponse   -> liftIO (hFlush h)
        CloseConn     -> liftIO (hClose h)
        >>= (viewT . cont >=> eval)

runSocketRpcServer :: (SocketServerMonad m, MonadIO m) => PortID -> RpcServer m a -> IO ()
runSocketRpcServer port server = do
    socket <- listenOn port
    let acceptLoop = do
            (handle, _, _) <- accept socket
            void $ forkIO $ serveClient handle
            acceptLoop

        byteServer = rpcByteConnection server
        serveClient = toIO . flip runSocketByteConnection byteServer

    acceptLoop
