module Network.HTTP3.Config where

import Network.HTTP2.Internal
import qualified System.TimeManager as T

data Config = Config {
    confPositionReadMaker :: PositionReadMaker
  , confTimeoutManager :: T.Manager
  }

allocSimpleConfig :: IO Config
allocSimpleConfig = Config defaultPositionReadMaker <$> T.initialize (30 * 1000000)

freeSimpleConfig :: Config -> IO ()
freeSimpleConfig conf = T.killManager $ confTimeoutManager conf
