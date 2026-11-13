{-# LANGUAGE OverloadedStrings #-}

module HTTP3.FileSpec where

import Control.Concurrent
import qualified Control.Exception as E
import Network.HTTP.Types
import qualified Network.HTTP3.Client as C
import Network.HTTP3.Server
import qualified Network.QUIC.Client as QUIC
import System.Timeout (timeout)
import Test.Hspec

import HTTP3.Config
import HTTP3.Server (setupWith, teardown)

spec :: Spec
spec =
    describe "H3 server sending a file" $
        it "closes the file when the client stops reading it" $ do
            closed <- newEmptyMVar
            E.bracket (setupWith (endless closed) fileServer 4096) teardown $ \_ -> do
                QUIC.run testClientConfig $ \conn ->
                    E.bracket allocSimpleConfig freeSimpleConfig $ \conf ->
                        C.run conn testH3ClientConfig conf $ \sendRequest _aux ->
                            sendRequest (C.requestNoBody methodGet "/" []) $ \rsp ->
                                -- One chunk, and the stream is given up on.
                                () <$ C.getResponseBodyChunk rsp
                -- It was closed only at the end of the file, which this
                -- one never reaches: the descriptor was left open.
                timeout 2000000 (takeMVar closed) `shouldReturn` Just ()

-- | A file of 100 MB, whatever its name, whose closing fills the 'MVar'.
endless :: MVar () -> Config -> Config
endless closed conf = conf{confPositionReadMaker = mk}
  where
    mk _ = return (\_ n _ -> return n, Closer $ () <$ tryPutMVar closed ())

fileServer :: Server
fileServer _req _aux sendResponse =
    sendResponse (responseFile ok200 [] (FileSpec "endless" 0 100000000)) []
