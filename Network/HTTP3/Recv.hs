{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HTTP3.Recv (
    Source
  , newSource
  , recvHeader
  , recvBody
  ) where

import qualified Data.ByteString as BS
import Data.IORef
import Network.HPACK (HeaderTable)
import Network.QUIC

import Imports
import Network.HTTP3.Context
import Network.HTTP3.Frame

data Source = Source {
    sourceRead    :: IO ByteString
  , sourcePending :: IORef (Maybe ByteString)
  }

newSource :: Stream -> IO Source
newSource strm = Source (recvStream strm 1024) <$> newIORef Nothing

readSource :: Source -> IO ByteString
readSource Source{..} = do
    mx <- readIORef sourcePending
    case mx of
      Nothing -> sourceRead
      Just x  -> do
          writeIORef sourcePending Nothing
          return x

pushbackSource :: Source -> ByteString -> IO ()
pushbackSource Source{..} "" = return ()
pushbackSource Source{..} bs = writeIORef sourcePending $ Just bs

recvHeader :: Context -> Source -> IO (Maybe HeaderTable)
recvHeader ctx src = loop IInit
  where
    loop st = do
        bs <- readSource src
        if bs == "" then
            return Nothing
          else case parseH3Frame st bs of
                 IDone H3FrameHeaders payload leftover -> do
                     pushbackSource src leftover
                     Just <$> qpackDecode ctx payload
                 IDone _ _ leftover -> do -- greasing
                     pushbackSource src leftover
                     loop IInit
                 st' -> loop st'

recvBody :: Context -> Source -> IORef IFrame -> IORef (Maybe HeaderTable) -> IO ByteString
recvBody ctx src refI refH = do
    st <- readIORef refI
    loop st
  where
    loop st = do
        bs <- readSource src
        if bs == "" then
            return ""
          else case parseH3Frame st bs of
                 IDone H3FrameHeaders payload _leftover -> do
                     writeIORef refI IInit
                     -- pushbackSource src leftover -- fixme
                     hdr <- qpackDecode ctx payload
                     writeIORef refH $ Just hdr
                     return ""
                 IPay H3FrameData siz received bss -> do
                     let st' = IPay H3FrameData siz received []
                     if null bss then
                         loop st'
                       else do
                         writeIORef refI st'
                         return $ BS.concat $ reverse bss
                 IDone H3FrameData payload leftover -> do
                     writeIORef refI IInit
                     pushbackSource src leftover
                     return payload
                 st' -> loop st'
