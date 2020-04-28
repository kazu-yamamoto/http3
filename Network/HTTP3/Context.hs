{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HTTP3.Context (
    Context
  , newContext
  , unidirectional
  , isH3Server
  , isH3Client
  , accept
  , qpackEncode
  , qpackDecode
  ) where

import Control.Concurrent
import qualified Data.ByteString as BS
import Network.QUIC
import Network.QUIC.Connection (isServer, isClient)

import Imports
import Network.HTTP3.Stream
import Network.QPACK

data Context = Context {
    ctxConnection :: Connection
  , ctxQEncoder   :: QEncoder
  , ctxQDecoder   :: QDecoder
  , ctxUniSwitch  :: H3StreamType -> InstructionHandler
  }

newContext :: Connection -> InstructionHandler -> QEncoder -> InstructionHandler -> QDecoder -> InstructionHandler -> IO Context
newContext conn ctl enc handleDI dec handleEI = do
    let sw = switch ctl handleEI handleDI
    return $ Context conn enc dec sw

switch :: InstructionHandler -> InstructionHandler -> InstructionHandler -> H3StreamType -> InstructionHandler
switch ctl handleEI handleDI styp
  | styp == H3ControlStreams   = ctl
  | styp == QPACKEncoderStream = handleEI
  | styp == QPACKDecoderStream = handleDI
  | otherwise                  = \_ -> putStrLn "switch unknown stream type"

isH3Server :: Context -> Bool
isH3Server Context{..} = isServer ctxConnection

isH3Client :: Context -> Bool
isH3Client Context{..} = isClient ctxConnection

accept :: Context -> IO (Either QUICError Stream)
accept Context{..} = acceptStream ctxConnection

qpackEncode :: Context -> QEncoder
qpackEncode Context{..} = ctxQEncoder

qpackDecode :: Context -> QDecoder
qpackDecode Context{..} = ctxQDecoder

unidirectional :: Context -> Stream -> IO ()
unidirectional Context{..} strm = do
    [w8] <- BS.unpack <$> recvStream strm 1 -- fixme: variable length
    let typ = toH3StreamType $ fromIntegral w8
    void $ forkIO $ ctxUniSwitch typ (recvStream strm)
