{-# LANGUAGE GADTs #-}

module Network.JsonRpc2.Server.Mapper where

import Data.Maybe
import Data.Aeson (Value(Null))
import Data.Text (Text)
import qualified Data.HashMap.Strict as H

import Network.JsonRpc2.Request
import Network.JsonRpc2.Response
import Network.JsonRpc2.Error
import Network.JsonRpc2.Server.Function

data Mapping m where
    (:=>) :: ToFunction m f => Text -> f -> Mapping m

mapper :: Monad m => [Mapping m] -> (Request -> m Response)
mapper maps = go where
    go req@(Request name _ rqid) = case H.lookup name hash of
        Nothing -> return $ Failure methodNotFound rsid
        Just f  -> f req
        where rsid = fromMaybe Null rqid

    hash = H.fromList $ map toTuple maps
    toTuple (name :=> tf) = (name, fun tf)
