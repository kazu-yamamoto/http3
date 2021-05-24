{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}

module ServerX where

import qualified Network.HQ.Server as HQ
import qualified Network.HTTP.Types as H
import qualified Network.HTTP3.Server as H3
import qualified UnliftIO.Exception as E

import Network.QUIC

serverHQ :: Connection -> IO ()
serverHQ = serverX HQ.run

serverH3 :: Connection -> IO ()
serverH3 = serverX H3.run

serverX :: (Connection -> H3.Config -> H3.Server -> IO ()) -> Connection -> IO ()
serverX run conn = E.bracket H3.allocSimpleConfig H3.freeSimpleConfig $ \conf ->
  run conn conf $ \_req _aux sendResponse -> do
    let hdr = [ ("Content-Type", "text/html; charset=utf-8")
              , ("Server", "HaskellQuic/0.0.0")
              ]
        rsp = H3.responseBuilder H.ok200 hdr "Hello, world!"
    sendResponse rsp []
