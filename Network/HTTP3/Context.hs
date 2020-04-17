{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HTTP3.Context (
    Context
  , newContext
  , unidirectional
  , isH3Server
  , isH3Client
  , send
  , recv
  , qpackEncode
  , qpackDecode
  ) where

import Data.IORef
import Data.IntMap (IntMap)
import qualified Data.IntMap as I
import Network.QUIC
import Network.QUIC.Connection

import Imports
import Network.HTTP3.Frame
import Network.HTTP3.Stream
import Network.QPACK

data Context = Context {
    ctxConnection :: Connection
  , ctxQEncoder   :: QEncoder
  , ctxQDecoder   :: QDecoder
  , ctxUniSwitch  :: H3StreamType -> Handle
  , ctxUniMap     :: IORef (IntMap (Either QInt Handle))
  }

newContext :: Connection -> Handle -> QEncoder -> HandleInstruction -> QDecoder -> HandleInstruction -> IO Context
newContext conn ctl enc handleDI dec handleEI = do
    let sw = switch ctl handleEI handleDI
    Context conn enc dec sw <$> newIORef I.empty

type Handle = ByteString -> IO ()

switch :: Handle -> Handle -> Handle -> H3StreamType -> Handle
switch ctl handleEI handleDI styp
  | styp == H3ControlStreams   = ctl
  | styp == QPACKEncoderStream = handleEI
  | styp == QPACKDecoderStream = handleDI
  | otherwise                  = \_ -> putStrLn "switch unknown stream type"

lookupUniMap :: Context -> StreamId -> IO (Maybe (Either QInt Handle))
lookupUniMap Context{..} sid = I.lookup sid <$> readIORef ctxUniMap

storeUniMap :: Context -> StreamId -> QInt -> IO ()
storeUniMap Context{..} sid qst = do
    m <- I.insert sid (Left qst) <$> readIORef ctxUniMap
    writeIORef ctxUniMap m

registerUniMap :: Context -> StreamId -> H3StreamType -> IO Handle
registerUniMap Context{..} sid typ = do
    let handler = ctxUniSwitch typ
    m <- I.insert sid (Right handler) <$> readIORef ctxUniMap
    writeIORef ctxUniMap m
    return handler

isH3Server :: Context -> Bool
isH3Server Context{..} = isServer ctxConnection

isH3Client :: Context -> Bool
isH3Client Context{..} = isClient ctxConnection

send :: Context -> StreamId -> ByteString -> Fin -> IO ()
send Context{..} = sendStream ctxConnection

recv :: Context -> IO (StreamId, ByteString, Fin)
recv Context{..} = recvStream ctxConnection

qpackEncode :: Context -> QEncoder
qpackEncode Context{..} = ctxQEncoder

qpackDecode :: Context -> QDecoder
qpackDecode Context{..} = ctxQDecoder

unidirectional :: Context -> StreamId -> ByteString -> IO ()
unidirectional _   _   "" = return ()
unidirectional ctx sid bs0 = do
    mex <- lookupUniMap ctx sid
    case mex of
      Nothing          -> register QInit bs0
      Just (Left  qst) -> register qst   bs0
      Just (Right uni) -> uni bs0
  where
    register qst bs = do
        case parseQInt qst bs of
          QDone i bs' -> do
              let typ = toH3StreamType i
              uni <- registerUniMap ctx sid typ
              uni bs'
          qst' -> storeUniMap ctx sid qst'
