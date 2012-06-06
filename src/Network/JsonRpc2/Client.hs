{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}

module Network.JsonRpc2.Client where

import Network.JsonRpc2.Request

import Control.Applicative
import Control.Monad.IO.Class
import Data.Aeson
import Data.Text (Text)
import qualified Data.Vector as V

newtype RpcSession m a = RpcSession { runRpc :: m a }

instance Functor m => Functor (RpcSession m) where
    fmap f (RpcSession rpc) = RpcSession (fmap f rpc)

instance Applicative m => Applicative (RpcSession m) where
    pure = RpcSession . pure
    RpcSession s1 <*> RpcSession s2 = RpcSession (s1 <*> s2)

instance Monad m => Monad (RpcSession m) where
    return = RpcSession . return

    RpcSession rpc >>= f = RpcSession $ rpc >>= runRpc . f

class Monad c => RpcClientMonad c where
    newReqId :: c Int
    execRequest :: FromJSON r => Request -> c r

class RpcCall c where
    makeReq :: Bool -> Text -> [Value] -> c

instance (ToJSON a, RpcCall r) => RpcCall (a -> r) where
    makeReq notify method params a = makeReq notify method (toJSON a:params)

instance (RpcClientMonad m, FromJSON a) => RpcCall (RpcSession m a) where
    makeReq notify method params = RpcSession $ do
        let arrpar = ArrayParams $ V.fromList $ reverse params
        if notify
            then do
                rqid <- newReqId
                execRequest $ Request method arrpar (Just $ toJSON rqid)
            else execRequest $ Request method arrpar Nothing

call :: RpcCall c => Text -> c
call method = makeReq False method []

