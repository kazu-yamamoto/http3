{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HTTP3.Recv (
    Source,
    newSource,
    readSource,
    readSource',
    recvHeader,
    newBodyReader,
) where

import qualified Control.Exception as E
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.IORef
import Network.QUIC

import Imports
import Network.HTTP3.Context
import Network.HTTP3.Error
import Network.HTTP3.Frame

data Source = Source
    { sourceRead :: IO ByteString
    , sourcePending :: IORef (Maybe ByteString)
    }

newSource :: Stream -> IO Source
newSource strm = Source (recvStream strm 1024) <$> newIORef Nothing

readSource :: Source -> IO ByteString
readSource Source{..} = do
    mx <- readIORef sourcePending
    case mx of
        Nothing -> sourceRead
        Just x -> do
            writeIORef sourcePending Nothing
            return x

readSource' :: Source -> IO (ByteString, Bool)
readSource' src = do
    x <- readSource src
    return $ if x == "" then (x, True) else (x, False)

pushbackSource :: Source -> ByteString -> IO ()
pushbackSource _ "" = return ()
pushbackSource Source{..} bs = writeIORef sourcePending $ Just bs

recvHeader :: Context -> StreamId -> Source -> IO (Maybe TokenHeaderTable)
recvHeader ctx sid src = loop IInit
  where
    lim = getMaxFieldSectionSize ctx
    loop st = do
        bs <- readSource src
        if bs == ""
            then return Nothing
            else case parseH3Frame lim st bs of
                ITooLong _ _ -> do
                    abort ctx H3ExcessiveLoad
                    loop IInit -- dummy
                st0
                    -- Nothing here drains DATA, so it is not covered by the
                    -- length cap; and it is not allowed before HEADERS
                    -- anyway.  Refuse it as soon as the type is known, rather
                    -- than after buffering whatever length it claimed.
                    | Just H3FrameData <- frameTypeOf st0 -> do
                        abort ctx H3FrameUnexpected
                        loop IInit -- dummy
                IDone typ payload leftover
                    | typ == H3FrameHeaders -> do
                        pushbackSource src leftover
                        Just <$> qpackDecode ctx sid payload
                    | permittedInRequestStream typ -> do
                        pushbackSource src leftover
                        loop IInit
                    | otherwise -> do
                        abort ctx H3FrameUnexpected
                        loop IInit -- dummy
                st' -> loop st'

-- | A body reader for one message, and the place its trailers will appear.
--
-- The reader counts what it hands out and checks the total against
-- content-length when the body ends, since a message whose content does not
-- match what it declared is malformed (RFC 9114, section 4.1.2).
--
-- Only what is actually read is counted, so a body the application never asks
-- for is never checked. Answering that would mean draining it on the
-- application's behalf, which is a different design from the one here.
newBodyReader
    :: Context
    -> StreamId
    -> Source
    -> ValueTable
    -> IO (IO (ByteString, Bool), IORef (Maybe TokenHeaderTable))
newBodyReader ctx sid src vt = do
    refI <- newIORef IInit
    refH <- newIORef Nothing
    refL <- newIORef 0
    let mcl = fst <$> (getFieldValue tokenContentLength vt >>= C8.readInt)
    return (recvBody ctx sid src refI refH mcl refL, refH)

recvBody
    :: Context
    -> StreamId
    -> Source
    -> IORef IFrame
    -> IORef (Maybe TokenHeaderTable)
    -> Maybe Int
    -> IORef Int
    -> IO (ByteString, Bool)
recvBody ctx sid src refI refH mcl refL = do
    st <- readIORef refI
    loop st
  where
    lim = getMaxFieldSectionSize ctx
    endOfBody = do
        forM_ mcl $ \cl -> do
            len <- readIORef refL
            when (cl /= len) $ E.throwIO $ ContentLengthMismatch cl len
        return ("", True)
    chunk bs = do
        modifyIORef' refL (+ BS.length bs)
        return (bs, False)
    loop st = do
        bs <- readSource src
        if bs == ""
            then endOfBody
            else case parseH3Frame lim st bs of
                ITooLong _ _ -> do
                    abort ctx H3ExcessiveLoad
                    return ("", True) -- dummy
                IPay H3FrameData siz received bss -> do
                    let st' = IPay H3FrameData siz received []
                    if null bss
                        then loop st'
                        else do
                            writeIORef refI st'
                            chunk $ BS.concat $ reverse bss
                IDone typ payload leftover
                    | typ == H3FrameHeaders -> do
                        writeIORef refI IInit
                        -- pushbackSource src leftover -- fixme
                        hdr <- qpackDecode ctx sid payload
                        writeIORef refH $ Just hdr
                        endOfBody
                    | typ == H3FrameData -> do
                        writeIORef refI IInit
                        pushbackSource src leftover
                        chunk payload
                    | permittedInRequestStream typ -> do
                        pushbackSource src leftover
                        loop IInit
                    | otherwise -> do
                        abort ctx H3FrameUnexpected
                        return (payload, False) -- dummy
                st' -> loop st'
