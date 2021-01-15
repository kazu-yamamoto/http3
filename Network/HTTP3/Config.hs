module Network.HTTP3.Config where

import Network.HTTP2.Internal
import Network.HTTP3.Frame
import qualified System.TimeManager as T

data Hooks = Hooks {
    onControlFrameCreated :: [H3Frame] -> [H3Frame]
  , onHeadersFrameCreated :: [H3Frame] -> [H3Frame]
  }

defaultHooks :: Hooks
defaultHooks = Hooks {
    onControlFrameCreated = id
  , onHeadersFrameCreated = id
  }

-- | Configuration for HTTP\/3 or HQ.
data Config = Config {
    confHooks :: Hooks
  , confPositionReadMaker :: PositionReadMaker
  , confTimeoutManager :: T.Manager
  }

-- | Allocating a simple configuration with a handle-based position
--   reader and a locally allocated timeout manager.
allocSimpleConfig :: IO Config
allocSimpleConfig = Config defaultHooks defaultPositionReadMaker <$> T.initialize (30 * 1000000)

-- | Freeing a simple configration.
freeSimpleConfig :: Config -> IO ()
freeSimpleConfig conf = T.killManager $ confTimeoutManager conf
