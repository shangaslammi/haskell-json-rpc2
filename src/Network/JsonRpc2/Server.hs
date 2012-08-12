{-# LANGUAGE GADTs #-}

module Network.JsonRpc2.Server where

import Network.JsonRpc2.Request
import Network.JsonRpc2.Response
import Network.JsonRpc2.Error
import Network.JsonRpc2.ByteConnection
import Network.JsonRpc2.Server.Mapper

import Control.Monad
import Control.Monad.Operational
import Control.Monad.Trans.Class

import Data.Aeson (json', Value(..), fromJSON, encode)
import Data.Attoparsec.ByteString (parseWith, IResult(..))

import qualified Data.Aeson as JSON
import qualified Data.ByteString as B


data RpcServerInstr a where
    GetRequest   :: RpcServerInstr Request
    SendResponse :: Response -> RpcServerInstr ()

type RpcServer = ProgramT RpcServerInstr

getRequest :: RpcServer m Request
getRequest = singleton GetRequest

sendResponse :: Response -> RpcServer m ()
sendResponse = singleton . SendResponse

runByteConnectionServer :: Monad m => RpcServer m a -> ByteConnection m ()
runByteConnectionServer = lift . viewT >=> eval B.empty where
    eval _ (Return _) = closeConnection
    eval buf (instr :>>= cont) = case instr of
        GetRequest -> do
            r <- parseWith getSomeBytes json' buf
            case r of
                Done xtra js -> case fromJSON js of
                    JSON.Error _  -> do
                        respond $ Failure invalidRequest Null
                        closeConnection
                    JSON.Success req -> lift . viewT >=> eval xtra $ cont req
                _ -> do
                    respond $ Failure parseError Null
                    closeConnection
        SendResponse resp -> respond resp >>= (lift . viewT . cont >=> eval buf)

mappedServer :: (Monad m, Functor m) => (Request -> m Response) -> RpcServer m ()
mappedServer f = getRequest >>= responseFunc where
    responseFunc = lift . f >=> sendResponse

