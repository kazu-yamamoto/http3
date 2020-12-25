module Network.HTTP3.Config where

import Network.HTTP2.Internal
import qualified System.TimeManager as T

-- | Configuration for HTTP\/3 or HQ.
data Config = Config {
    confPositionReadMaker :: PositionReadMaker
  , confTimeoutManager :: T.Manager
  }

-- | Allocating a simple configuration with a handle-based position
--   reader and a locally allocated timeout manager.
allocSimpleConfig :: IO Config
allocSimpleConfig = Config defaultPositionReadMaker <$> T.initialize (30 * 1000000)

-- | Freeing a simple configration.
freeSimpleConfig :: Config -> IO ()
freeSimpleConfig conf = T.killManager $ confTimeoutManager conf
