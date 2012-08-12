{-# LANGUAGE OverloadedStrings #-}

import Network

import Network.JsonRpc2.Server
import Network.JsonRpc2.Server.Socket
import Network.JsonRpc2.Server.Mapper

add :: Double -> Double -> IO Double
add a b = return $ a + b

main = withSocketsDo $ do
    let services = mapper ["add" :=> add]
    runSocketRpcServer (PortNumber 8000) $ mappedServer services
