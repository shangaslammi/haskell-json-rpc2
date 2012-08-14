{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.JsonRpc2.Client where

import Network.JsonRpc2.Request
import Network.JsonRpc2.Response
import Network.JsonRpc2.Error
import Network.JsonRpc2.ByteConnection

import Control.Monad
import Control.Monad.Identity
import Control.Monad.Operational
import Control.Monad.Trans.Class

import Data.Aeson (FromJSON(..), ToJSON(..), fromJSON, json')
import Data.Attoparsec.ByteString (parseWith, IResult(..))
import Data.Text (Text)

import qualified Data.Aeson as JSON
import qualified Data.ByteString as B
import qualified Data.Vector as V
import qualified Data.IntMap as IntMap


data ClientInstr a where
    CallFunc :: Text -> RequestParams -> ClientInstr (Either Text Response)
    Notify   :: Text -> RequestParams -> ClientInstr ()

type RpcClientT = ProgramT ClientInstr
type RpcClient  = RpcClientT Identity

class RpcCall c where
    makeCall :: Text -> [JSON.Value] -> c

class RpcNotify n where
    makeNotify :: Text -> [JSON.Value] -> n

class Monad m => MonadRpc m where
    rpcCallFunc :: Text -> RequestParams -> m (Either Text Response)
    rpcNotify   :: Text -> RequestParams -> m ()

instance Monad m => MonadRpc (RpcClientT m) where
    rpcCallFunc = (.) singleton . CallFunc
    rpcNotify   = (.) singleton . Notify

instance (Monad (t m), MonadRpc m, MonadTrans t) => MonadRpc (t m) where
    rpcCallFunc = (.) lift . rpcCallFunc
    rpcNotify   = (.) lift . rpcNotify

instance (ToJSON a, RpcCall r) => RpcCall (a -> r) where
    makeCall method params a = makeCall method (toJSON a:params)

instance (MonadRpc m, FromJSON a) => RpcCall (m (Either Error a)) where
    makeCall method params = do
        let arrpar = ArrayParams $ V.fromList $ reverse params
        res <- rpcCallFunc method arrpar
        case res of
            Left msg -> return $ Left $ internalError msg
            Right (Success value _) -> case JSON.fromJSON value of
                JSON.Error _ -> return $ Left wrongResultType
                JSON.Success a -> return $ Right a
            Right (Failure err _) -> return $ Left err

instance (ToJSON a, RpcNotify r) => RpcNotify (a -> r) where
    makeNotify method params a = makeNotify method (toJSON a:params)

instance (MonadRpc m) => RpcNotify (m ()) where
    makeNotify method params = do
        let arrpar = ArrayParams $ V.fromList $ reverse params
        rpcNotify method arrpar


call :: RpcCall c => Text -> c
call method = makeCall method []

notify :: RpcNotify c => Text -> c
notify method = makeNotify method []

runByteConnectionClientT :: Monad m => RpcClientT m a -> ByteConnection m a
runByteConnectionClientT = lift . viewT >=> eval B.empty 0 where
    eval _ _ (Return a) = closeConnection >> return a
    eval buf reqId (instr :>>= cont) = case instr of
        CallFunc name params -> do
            let req = Request name params (Just $ toJSON reqId)
                reqId' = reqId + 1 :: Int
            request req
            r <- parseWith getSomeBytes json' buf
            case r of
                Done xtra js -> lift . viewT . cont >=> eval xtra reqId' $ case fromJSON js of
                    JSON.Error _  -> Left "server returned invalid JSON"
                    JSON.Success res ->  Right res
                _ -> lift . viewT . cont >=> eval B.empty reqId' $
                    Left "server returned an incomplete response"
        Notify name params -> do
            let req = Request name params Nothing
            request req >>= (lift . viewT . cont >=> eval buf reqId)
