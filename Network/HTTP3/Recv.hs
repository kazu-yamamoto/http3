{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HTTP3.Recv (
    Source,
    newSource,
    readSource,
    readSource',
    recvHeader,
    newBodyReader,
    cancelUnlessReadToEnd,
    connectionSpecificField,
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
    { sourceStream :: Stream
    , sourceRead :: IO ByteString
    , sourcePending :: IORef (Maybe ByteString)
    , sourceReadToEnd :: IORef Bool
    -- ^ Whether the body reader has come to the end of the stream between two
    -- frames, and so has processed every field section on it.
    }

newSource :: Stream -> IO Source
newSource strm =
    Source strm (recvStream strm 1024) <$> newIORef Nothing <*> newIORef False

-- | Telling the peer's QPACK encoder, unless the stream has been read to its
--   end, that the field sections left on it will never be processed (RFC 9204,
--   section 4.4.2): until it is told, it keeps the entries they refer to.
--
-- A stream the peer reset has not been read to its end, even if it looks so:
-- the reset reads as an end, and one that lands between two frames used to
-- be taken for the real thing.  Nothing is lost by telling an encoder about a
-- stream it has nothing outstanding on.
cancelUnlessReadToEnd :: Context -> StreamId -> Source -> IO ()
cancelUnlessReadToEnd ctx sid Source{..} = do
    done <- readIORef sourceReadToEnd
    reset <- isJust <$> resetReceived sourceStream
    when (not done || reset) $ cancelStream ctx sid

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

-- | The first connection-specific field in a field section, if any.
--
-- RFC 9114, section 4.2: "An endpoint MUST NOT generate an HTTP/3 field
-- section containing connection-specific fields; any message containing
-- connection-specific fields MUST be treated as malformed."  It names
-- Connection, Keep-Alive, Proxy-Connection, Transfer-Encoding and Upgrade,
-- and lets TE through only with the value "trailers".  Everything else a
-- Connection field could name is a connection-specific field too, but then
-- the Connection field is there to be refused.
connectionSpecificField :: TokenHeaderTable -> Maybe ByteString
connectionSpecificField (ths, vt)
    | isJust (getFieldValue tokenConnection vt) = Just "connection"
    | isJust (getFieldValue tokenTransferEncoding vt) = Just "transfer-encoding"
    | maybe False (/= "trailers") (getFieldValue tokenTE vt) = Just "te"
    | otherwise = find (`elem` byName) $ map (foldedCase . tokenKey . fst) ths
  where
    -- No tokens of their own.
    byName = ["keep-alive", "proxy-connection", "upgrade"]

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
            then do
                -- Not in the middle of a frame, which would mean it was cut
                -- off.
                when (st == IInit) $ writeIORef (sourceReadToEnd src) True
                endOfBody
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
                        forM_ (connectionSpecificField hdr) $
                            E.throwIO . ConnectionSpecificField
                        writeIORef refH $ Just hdr
                        endOfBody
                    | typ == H3FrameData -> do
                        writeIORef refI IInit
                        pushbackSource src leftover
                        -- A DATA frame may be empty (RFC 9114, section
                        -- 7.2.1), and "" is how the end of the body is told
                        -- to the reader.  Go on to the next frame instead.
                        if BS.null payload then loop IInit else chunk payload
                    | permittedInRequestStream typ -> do
                        pushbackSource src leftover
                        loop IInit
                    | otherwise -> do
                        abort ctx H3FrameUnexpected
                        return (payload, False) -- dummy
                st' -> loop st'
