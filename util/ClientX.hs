{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}

module ClientX where

import qualified Control.Exception as E
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import Network.HTTP.Types
import Network.QUIC

import qualified Network.HQ.Client as HQ
import qualified Network.HTTP3.Client as H3

data Aux = Aux {
    auxPath       :: String
  , auxAuthority  :: String
  , auxDebug      :: String -> IO ()
  , auxShow       :: ByteString -> IO ()
  , auxCheckClose :: IO Bool
  }

type Cli = Aux -> Connection -> IO ConnectionStats


clientHQ :: Cli
clientHQ = clientX HQ.run

clientH3 :: Cli
clientH3 = clientX H3.run

clientX ::  (Connection -> H3.ClientConfig -> H3.Config -> H3.Client ConnectionStats -> IO ConnectionStats) -> Cli
clientX run Aux{..} conn = E.bracket H3.allocSimpleConfig H3.freeSimpleConfig $ \conf -> run conn cliconf conf client
  where
    cliconf = H3.ClientConfig {
        scheme = "https"
      , authority = C8.pack auxAuthority
      }
    client sendRequest = do
        let req = H3.requestNoBody methodGet (C8.pack auxPath) [("User-Agent", "HaskellQuic/0.0.0")]
        sendRequest req $ \rsp -> do
            auxShow "------------------------"
            loop rsp
            auxShow "------------------------"
            getConnectionStats conn
    loop rsp = do
        x <- H3.getResponseBodyChunk rsp
        when (x /= "") $ do
            auxShow x
            loop rsp
