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
  , Input(..)
  , putInput
  , takeInput
  , OpenState(..)
  , RequestStream(..)
  , getRequestStream
  , getOpenState
  , setOpenState
  ) where

import Control.Concurrent.STM
import Data.IORef
import Data.IntMap (IntMap)
import qualified Data.IntMap as I
import Network.HPACK (HeaderTable)
import Network.HTTP2.Internal (InpObj(..))
import Network.QUIC
import Network.QUIC.Connection (isServer, isClient)

import Imports
import Network.HTTP3.Frame
import Network.HTTP3.Stream
import Network.QPACK

data Context = Context {
    ctxConnection :: Connection
  , ctxQEncoder   :: QEncoder
  , ctxQDecoder   :: QDecoder
  , ctxUniSwitch  :: H3StreamType -> Handle
  , ctxUniTable   :: IORef (IntMap (Either QInt Handle))
  , ctxReqTable   :: IORef (IntMap RequestStream)
  , inputQ        :: TQueue Input
  }

data Input = Input StreamId InpObj (Maybe (TQueue ByteString))

newContext :: Connection -> Handle -> QEncoder -> HandleInstruction -> QDecoder -> HandleInstruction -> IO Context
newContext conn ctl enc handleDI dec handleEI = do
    let sw = switch ctl handleEI handleDI
    Context conn enc dec sw <$> newIORef I.empty <*> newIORef I.empty <*> newTQueueIO

type Handle = ByteString -> IO ()

switch :: Handle -> Handle -> Handle -> H3StreamType -> Handle
switch ctl handleEI handleDI styp
  | styp == H3ControlStreams   = ctl
  | styp == QPACKEncoderStream = handleEI
  | styp == QPACKDecoderStream = handleDI
  | otherwise                  = \_ -> putStrLn "switch unknown stream type"

lookupUniMap :: Context -> StreamId -> IO (Maybe (Either QInt Handle))
lookupUniMap Context{..} sid = I.lookup sid <$> readIORef ctxUniTable

storeUniMap :: Context -> StreamId -> QInt -> IO ()
storeUniMap Context{..} sid qst = do
    m <- I.insert sid (Left qst) <$> readIORef ctxUniTable
    writeIORef ctxUniTable m

registerUniMap :: Context -> StreamId -> H3StreamType -> IO Handle
registerUniMap Context{..} sid typ = do
    let handler = ctxUniSwitch typ
    m <- I.insert sid (Right handler) <$> readIORef ctxUniTable
    writeIORef ctxUniTable m
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

data OpenState =
    JustOpened
  | Continued (IORef IFrame)
  | NoBody HeaderTable
  | HasBody HeaderTable (TQueue ByteString) (IORef IFrame)
  | Body (TQueue ByteString)
         (IORef IFrame)
         (Maybe Int) -- received Content-Length
                     -- compared the body length for error checking
         (IORef Int) -- actual body length
         (IORef (Maybe HeaderTable)) -- trailers

instance Show OpenState where
    show JustOpened  = "JustOpened"
    show Continued{} = "Continued"
    show NoBody{}    = "NoBody"
    show HasBody{}   = "HasBody"
    show Body{}      = "Body"

data RequestStream = RequestStream StreamId (IORef OpenState)

newRequestStream :: StreamId -> IO RequestStream
newRequestStream sid = RequestStream sid <$> newIORef JustOpened

putInput :: Context -> Input -> IO ()
putInput Context{..} inp = atomically $ writeTQueue inputQ inp

takeInput :: Context -> IO Input
takeInput Context{..} = atomically $ readTQueue inputQ

getRequestStream :: Context -> StreamId -> IO RequestStream
getRequestStream Context{..} sid = do
    m <- readIORef ctxReqTable
    let mx = I.lookup sid m
    case mx of
      Just rs -> return rs
      Nothing -> do
          newrs <- newRequestStream sid
          let m' = I.insert sid newrs m
          writeIORef ctxReqTable m'
          return newrs

getOpenState :: RequestStream -> IO OpenState
getOpenState (RequestStream _ ref) = readIORef ref

setOpenState :: RequestStream -> OpenState -> IO ()
setOpenState (RequestStream _ ref) st = writeIORef ref st
