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
                 IPay typ siz received bss -> do
                     writeIORef refI $ IPay typ siz received []
                     return $ BS.concat $ reverse bss
                 IDone H3FrameHeaders payload leftover -> do
                     writeIORef refI IInit
                     pushbackSource src leftover
                     recvHeader ctx src >>= writeIORef refH
                     return payload
                 st' -> loop st'
