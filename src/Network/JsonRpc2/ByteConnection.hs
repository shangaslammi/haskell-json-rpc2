{-# LANGUAGE GADTs #-}

module Network.JsonRpc2.ByteConnection where

import Network.JsonRpc2.Response

import Data.ByteString (ByteString)
import Data.Aeson (encode)

import Control.Monad.Operational

import qualified Data.ByteString.Lazy as BL


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

