module Network.HTTP3.Config where

import Network.HTTP2.Internal

newtype Config = Config {
    confPositionReadMaker :: PositionReadMaker
  }

defaultConfig :: Config
defaultConfig = Config {
    confPositionReadMaker = defaultPositionReadMaker
  }
