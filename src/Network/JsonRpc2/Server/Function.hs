{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}

module Network.JsonRpc2.Server.Function where

import Control.Monad
import Data.Aeson as A
import Data.Maybe
import Data.Vector ((!))
import qualified Data.Vector as V

import Network.JsonRpc2.Request
import Network.JsonRpc2.Response as R
import Network.JsonRpc2.Error

fun :: ToFunction m f => f -> (Request -> m Response)
fun = constrFunc 0

class ToFunction m f where
    constrFunc :: Int -> f -> (Request -> m Response)

instance (Monad m, ToJSON b) => ToFunction m (m b) where
    constrFunc idx f req@(Request _ (ArrayParams params) rqid)
        | tooManyParams = paramsError
        | otherwise = do
            retval <- liftM toJSON f
            return $ R.Success retval rsid
        where
            tooManyParams = V.length params > idx
            paramsError = return $ Failure invalidParams rsid
            rsid = fromMaybe Null rqid

    constrFunc _ _ (Request _ _ rqid) = paramsError where
        paramsError = return $ Failure invalidParams rsid
        rsid = fromMaybe Null rqid


instance (Monad m, FromJSON a, ToFunction m r) => ToFunction m (a -> r) where
    constrFunc idx f req@(Request _ (ArrayParams params) rqid)
        | tooFewParams = paramsError
        | otherwise = case a of
            A.Error _   -> paramsError
            A.Success v -> constrFunc (idx+1) (f v) req
        where
            tooFewParams = V.length params <= idx
            paramsError = return $ Failure invalidParams rsid
            a = fromJSON (params ! idx)
            rsid = fromMaybe Null rqid

    constrFunc _ _ (Request _ _ rqid) = paramsError where
        paramsError = return $ Failure invalidParams rsid
        rsid = fromMaybe Null rqid

test :: Int -> Int -> IO Int
test a b = return (a + b)

