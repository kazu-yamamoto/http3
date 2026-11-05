{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module QPACK.QIF2Spec where

import Control.Concurrent
import qualified Control.Exception as E
import Control.Monad
import qualified Data.ByteString as B
import Data.ByteString.Builder (Builder, byteString)
import qualified Data.ByteString.Char8 as C8
import Data.CaseInsensitive hiding (map)
import Data.List
import Data.Maybe
import Network.HTTP.Semantics
import Network.HTTP.Semantics.Client.Internal
import Network.HTTP.Semantics.Server
import Network.HTTP.Types (RequestHeaders, ok200)
import qualified Network.HTTP3.Client as C
import Network.HTTP3.Server
import qualified Network.HTTP3.Server as S
import qualified Network.QUIC.Client as QUIC
import System.IO
import Test.Hspec

import HTTP3.Config
import HTTP3.Server hiding (server)
import QIF

qiffile :: FilePath
qiffile = "qifs/qifs/fb-req-hq.qif"

spec :: Spec
spec = do
    describe "QIF server" $ do
        it "handles dynamic table of 0-bytes" $ do
            let size = 0
            E.bracket (setup server size) teardown $
                \_ -> runClient size
    describe "QIF server" $ do
        it "handles dynamic table of 256-bytes" $ do
            let size = 256
            E.bracket (setup server size) teardown $
                \_ -> runClient size
    describe "QIF server" $ do
        it "handles dynamic table of 512-bytes" $ do
            let size = 512
            E.bracket (setup server size) teardown $
                \_ -> runClient size
    describe "QIF server" $ do
        it "handles dynamic table of 4096-bytes" $ do
            let size = 4096
            E.bracket (setup server size) teardown $
                \_ -> runClient size

runClient :: Int -> IO ()
runClient siz = QUIC.run testClientConfig $ \conn ->
    E.bracket allocSimpleConfig freeSimpleConfig $ \conf0 -> do
        let conf =
                conf0
                    { S.confQEncoderConfig = S.defaultQEncoderConfig{S.ecMaxTableCapacity = siz}
                    , S.confQDecoderConfig = S.defaultQDecoderConfig{S.dcMaxTableCapacity = siz}
                    }

        withFile qiffile ReadMode $ \hdl ->
            C.run conn testH3ClientConfig conf $ client hdl

client :: Handle -> (C.Request -> (C.Response -> IO ()) -> IO a) -> p -> IO ()
client hdl sendRequest _aux = do
    -- delay for set table capacity
    threadDelay 10000
    ms <- forkClient 0 id
    -- A failure in a request's thread is brought back here.  It used to
    -- kill the thread before it said it was done, and the test hung.
    mapM takeMVar ms >>= mapM_ (either E.throwIO return)
  where
    forkClient n build = do
        hs <- headerlist hdl
        if null hs
            then return $ build []
            else do
                var <- newEmptyMVar
                _ <- forkIO $ do
                    let req = requestBuilder2 hs mempty
                    r <- E.try @E.SomeException $ sendRequest req $ checkHeader hs
                    putMVar var $ void r
                when ((n :: Int) `mod` 50 == 0) $ threadDelay 100000
                forkClient (n + 1) (build . (var :))
    checkHeader hs rsp = do
        bs <- getBody
        -- Every field but the pseudo-header ones, which the client may
        -- add to or rewrite.  This compared fields named "cookie:", which
        -- there are none of, and so compared two empty lists.
        -- Cookies are split into crumbs on both sides: the server joins
        -- them with "; " (RFC 9114, section 4.2.1).
        let regular = sort . concatMap crumbs . filter ((/= ":") . B.take 1 . foldedCase . fst)
            crumbs (k, v)
                | k == "cookie" = map ((,) k) $ splitOn "; " v
                | otherwise = [(k, v)]
            hs0 = map keyValue $ split bs
        regular hs0 `shouldBe` regular hs
      where
        getBody = B.concat <$> loop id
        loop build = do
            bs <- C.getResponseBodyChunk rsp
            if bs == ""
                then return $ build []
                else loop (build . (bs :))
        split ls0 = go ls0 id
          where
            go "" build = build []
            go ls build = go ls'' (build . (l :))
              where
                (l, ls') = C8.break (== '\n') ls
                ls'' = C8.drop 1 ls'
        splitOn sep s = case B.breakSubstring sep s of
            (x, "") -> [x]
            (x, rest) -> x : splitOn sep (B.drop (B.length sep) rest)
        keyValue kv = (mk k, v)
          where
            (k, v') = C8.break (== ' ') kv
            v = C8.drop 1 v'

-- | Answering with the fields of the request, whatever its method: the QIF
-- file has POSTs as well as GETs.
server :: Server
server req _aux sendResponse = sendResponse (responseSection req) []

responseSection :: S.Request -> S.Response
responseSection req = responseBuilder ok200 header body
  where
    header =
        [ ("Content-Type", "text/plain")
        , ("Server", "HaskellQuic/0.0.0")
        ]
    (thl, vt) = requestHeaders req
    pseudos = [tokenAuthority, tokenMethod, tokenPath, tokenScheme]
    pthl = map (\t -> (t, fromJust (getFieldValue t vt))) pseudos
    body =
        foldr1 (<>) $
            map (\(k, v) -> byteString (k <> " " <> v <> "\n")) $
                map (\(k, v) -> (tokenFoldedKey k, v)) (pthl ++ thl)

-- | Creating request with builder only from headers
--   where :path and :method are included.
requestBuilder2 :: RequestHeaders -> Builder -> C.Request
requestBuilder2 hdr builder = Request $ OutObj hdr (OutBodyBuilder builder) defaultTrailersMaker
