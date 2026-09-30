module HTTP3.ErrorSpec where

import Data.ByteString ()
import Data.IORef
import Test.Hspec

import HTTP3.Config
import HTTP3.Error
import HTTP3.Server

-- | The server counts the requests it is handed, so that a test can check a
-- rejected one never got that far.
spec :: Spec
spec =
    beforeAll start $
        afterAll (teardown . fst) $
            -- 2 seconds
            h3ErrorSpec testClientConfig testH3ClientConfig 2000 served
                >> h3DrainSpec testClientConfig testH3ClientConfig 2000
  where
    served (_, ref) = Just $ readIORef ref
    start = do
        ref <- newIORef 0
        tid <- setup (countingServer ref) 4096
        return (tid, ref)
