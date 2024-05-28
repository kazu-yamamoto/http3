{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HTTP3.Recv (
    Source,
    newSource,
    readSource,
    readSource',
    recvHeader,
    recvBody,
) where

import qualified Data.ByteString as BS
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

recvHeader :: Context -> Source -> IO (Maybe TokenHeaderTable)
recvHeader ctx src = loop IInit
  where
    loop st = do
        bs <- readSource src
        if bs == ""
            then return Nothing
            else case parseH3Frame st bs of
                IDone typ payload leftover
                    | typ == H3FrameHeaders -> do
                        pushbackSource src leftover
                        Just <$> qpackDecode ctx payload
                    | typ == H3FrameData -> do
                        abort ctx H3FrameUnexpected
                        loop IInit -- dummy
                    | permittedInRequestStream typ -> do
                        pushbackSource src leftover
                        loop IInit
                    | otherwise -> do
                        abort ctx H3FrameUnexpected
                        loop IInit -- dummy
                st' -> loop st'

recvBody
    :: Context
    -> Source
    -> IORef IFrame
    -> IORef (Maybe TokenHeaderTable)
    -> IO (ByteString, Bool)
recvBody ctx src refI refH = do
    st <- readIORef refI
    loop st
  where
    loop st = do
        bs <- readSource src
        if bs == ""
            then return ("", True)
            else case parseH3Frame st bs of
                IPay H3FrameData siz received bss -> do
                    let st' = IPay H3FrameData siz received []
                    if null bss
                        then loop st'
                        else do
                            writeIORef refI st'
                            let ret = BS.concat $ reverse bss
                            return (ret, False)
                IDone typ payload leftover
                    | typ == H3FrameHeaders -> do
                        writeIORef refI IInit
                        -- pushbackSource src leftover -- fixme
                        hdr <- qpackDecode ctx payload
                        writeIORef refH $ Just hdr
                        return ("", True)
                    | typ == H3FrameData -> do
                        writeIORef refI IInit
                        pushbackSource src leftover
                        return (payload, False)
                    | permittedInRequestStream typ -> do
                        pushbackSource src leftover
                        loop IInit
                    | otherwise -> do
                        abort ctx H3FrameUnexpected
                        return (payload, False) -- dummy
                st' -> loop st'
