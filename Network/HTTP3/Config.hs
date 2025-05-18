module Network.HTTP3.Config where

import Network.HTTP.Semantics.Client
import Network.HTTP3.Frame
import Network.QPACK
import Network.QUIC (Stream)
import qualified System.TimeManager as T

-- | Hooks mainly for error testing.
data Hooks = Hooks
    { onControlFrameCreated :: [H3Frame] -> [H3Frame]
    , onHeadersFrameCreated :: [H3Frame] -> [H3Frame]
    , onControlStreamCreated :: Stream -> IO ()
    , onEncoderStreamCreated :: Stream -> IO ()
    , onDecoderStreamCreated :: Stream -> IO ()
    }

-- | Default hooks.
defaultHooks :: Hooks
defaultHooks =
    Hooks
        { onControlFrameCreated = id
        , onHeadersFrameCreated = id
        , onControlStreamCreated = \_ -> return ()
        , onEncoderStreamCreated = \_ -> return ()
        , onDecoderStreamCreated = \_ -> return ()
        }

-- | Configuration for HTTP\/3 or HQ.
data Config = Config
    { confHooks :: Hooks
    , confQEncoderConfig :: QEncoderConfig
    , confQDecoderConfig :: QDecoderConfig
    , confPositionReadMaker :: PositionReadMaker
    , confTimeoutManager :: T.Manager
    }

defaultConfig :: Config
defaultConfig =
    Config
        { confHooks = defaultHooks
        , confQEncoderConfig = defaultQEncoderConfig
        , confQDecoderConfig = defaultQDecoderConfig
        , confPositionReadMaker = defaultPositionReadMaker
        , confTimeoutManager = T.defaultManager
        }

-- | Allocating a simple configuration with a handle-based position
--   reader and a locally allocated timeout manager.
allocSimpleConfig :: IO Config
allocSimpleConfig =
    Config
        defaultHooks
        defaultQEncoderConfig
        defaultQDecoderConfig
        defaultPositionReadMaker
        <$> T.initialize (30 * 1000000)

-- | Freeing a simple configration.
freeSimpleConfig :: Config -> IO ()
freeSimpleConfig conf = T.killManager $ confTimeoutManager conf
