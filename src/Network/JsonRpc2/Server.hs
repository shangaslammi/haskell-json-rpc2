{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Network.JsonRpc2.Server where

import Network.JsonRpc2.Request
import Network.JsonRpc2.Response
import Network.JsonRpc2.Error
import Network.JsonRpc2.Server.Mapper

import Control.Monad
import Control.Monad.Operational
import Control.Monad.Trans.Class

import Data.Aeson (json', Value(..), fromJSON, encode)
import qualified Data.Aeson as A

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Data.Attoparsec.ByteString (parseWith, IResult(..))

type ByteConnection = ProgramT ByteConnInstr

data ByteConnInstr a where
    GetSomeBytes :: ByteConnInstr ByteString
    WriteBytes   :: ByteString -> ByteConnInstr ()
    EndResponse  :: ByteConnInstr ()
    CloseConn    :: ByteConnInstr ()

getSomeBytes :: ByteConnection m ByteString
getSomeBytes = singleton GetSomeBytes

writeBytes :: ByteString -> ByteConnection m ()
writeBytes = singleton . WriteBytes

endResponse :: ByteConnection m ()
endResponse = singleton EndResponse

closeConnection :: ByteConnection m ()
closeConnection = singleton CloseConn

respond :: Monad m => Response -> ByteConnection m ()
respond = (>> endResponse) . mapM_ writeBytes . BL.toChunks . encode

data RpcServerInstr a where
    GetRequest   :: RpcServerInstr Request
    SendResponse :: Response -> RpcServerInstr ()

type RpcServer = ProgramT RpcServerInstr

getRequest :: RpcServer m Request
getRequest = singleton GetRequest

sendResponse :: Response -> RpcServer m ()
sendResponse = singleton . SendResponse

rpcByteConnection :: Monad m => RpcServer m a -> ByteConnection m ()
rpcByteConnection = lift . viewT >=> eval B.empty where
    eval _ (Return _) = closeConnection
    eval buf (instr :>>= cont) = case instr of
        GetRequest -> do
            r <- parseWith getSomeBytes json' buf
            case r of
                Done xtra js -> case fromJSON js of
                    A.Error _  -> do
                        respond $ Failure invalidRequest Null
                        closeConnection
                    A.Success req -> lift . viewT >=> eval xtra $ cont req
                _ -> do
                    respond $ Failure parseError Null
                    closeConnection
        SendResponse resp -> respond resp >>= (lift . viewT . cont >=> eval buf)

mappedServer :: (Monad m, Functor m) => (Request -> m Response) -> RpcServer m ()
mappedServer f = getRequest >>= responseFunc where
    responseFunc = lift . f >=> sendResponse

