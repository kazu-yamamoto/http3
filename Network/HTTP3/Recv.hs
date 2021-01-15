{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HTTP3.Recv (
    Source
  , newSource
  , readSource
  , recvHeader
  , recvBody
  ) where

import qualified Data.ByteString as BS
import Data.IORef
import Network.HPACK (HeaderTable)
import Network.QUIC

import Imports
import Network.HTTP3.Context
import Network.HTTP3.Error
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
pushbackSource _ "" = return ()
pushbackSource Source{..} bs = writeIORef sourcePending $ Just bs

recvHeader :: Context -> Source -> IO (Maybe HeaderTable)
recvHeader ctx src = loop IInit
  where
    loop st = do
        bs <- readSource src
        if bs == "" then
            return Nothing
          else
            case parseH3Frame st bs of
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

recvBody :: Context -> Source -> IORef IFrame -> IORef (Maybe HeaderTable) -> IO ByteString
recvBody ctx src refI refH = do
    st <- readIORef refI
    loop st
  where
    loop st = do
        bs <- readSource src
        if bs == "" then
            return ""
          else
            case parseH3Frame st bs of
              IPay H3FrameData siz received bss -> do
                  let st' = IPay H3FrameData siz received []
                  if null bss then
                      loop st'
                    else do
                      writeIORef refI st'
                      return $ BS.concat $ reverse bss
              IDone typ payload leftover
                | typ == H3FrameHeaders -> do
                      writeIORef refI IInit
                      -- pushbackSource src leftover -- fixme
                      hdr <- qpackDecode ctx payload
                      writeIORef refH $ Just hdr
                      return ""
                | typ == H3FrameData -> do
                      writeIORef refI IInit
                      pushbackSource src leftover
                      return payload
                | permittedInRequestStream typ -> do
                      pushbackSource src leftover
                      loop IInit
                | otherwise -> do
                      abort ctx H3FrameUnexpected
                      return payload -- dummy
              st' -> loop st'
