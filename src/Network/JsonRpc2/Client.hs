{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Network.JsonRpc2.Client where

import Network.JsonRpc2.Request
import Network.JsonRpc2.Response
import Network.JsonRpc2.Error
import Network.JsonRpc2.ByteConnection

import Control.Monad
import Control.Monad.Operational
import Control.Monad.Trans.Class

import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Text (Text)

import qualified Data.Aeson as JSON
import qualified Data.ByteString as B
import qualified Data.Vector as V


data ClientInstr a where
    CallFunc :: Text -> RequestParams -> ClientInstr Response
    Notify   :: Text -> RequestParams -> ClientInstr ()

type RpcClient = ProgramT ClientInstr

class RpcCall c where
    makeCall :: Text -> [JSON.Value] -> c

class RpcNotify n where
    makeNotify :: Text -> [JSON.Value] -> n

instance (ToJSON a, RpcCall r) => RpcCall (a -> r) where
    makeCall method params a = makeCall method (toJSON a:params)

instance (Monad m, FromJSON a) => RpcCall (RpcClient m (Either Error a)) where
    makeCall method params = do
        let arrpar = ArrayParams $ V.fromList $ reverse params
        res <- singleton $ CallFunc method arrpar
        case res of
            Success value _ -> case JSON.fromJSON value of
                JSON.Error _ -> return $ Left wrongResultType
                JSON.Success a -> return $ Right a

instance (ToJSON a, RpcNotify r) => RpcNotify (a -> r) where
    makeNotify method params a = makeNotify method (toJSON a:params)

instance RpcNotify (RpcClient m ()) where
    makeNotify method params = do
        let arrpar = ArrayParams $ V.fromList $ reverse params
        singleton $ Notify method arrpar


call :: RpcCall c => Text -> c
call method = makeCall method []

notify :: RpcNotify c => Text -> c
notify method = makeNotify method []

runByteConnectionClient :: Monad m => RpcClient m a -> ByteConnection m a
runByteConnectionClient = lift . viewT >=> eval B.empty where
    eval buf (Return a) = closeConnection >> return a
    eval buf (instr :>>= cont) = undefined -- TODO
