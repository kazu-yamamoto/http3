module Network.QPACK.Token (
    quicIx
  ) where

import Data.Array
import Data.Array.Base (unsafeAt)

hpack2QpackList :: [Int]
hpack2QpackList = [0,15,1,16,17,-1,19,31,20,18,22,2,-1,38,23,3,24,-1,4,-1,-1,25,5,6,7,-1,-1,-1,-1,-1,8,9,43,-1,10,11,12,-1,-1,-1,26,13,-1,-1,46,14,27,-1,49,28,-1,-1,-1,-1,32,21,33,34,35,36,37,39,40,41,42,44,45,47,48,29,50,51,30,-1]

hpack2QpackTable :: Array Int Int
hpack2QpackTable = listArray (0,length hpack2QpackList - 1) hpack2QpackList

quicIx :: Int -> Int
quicIx ix = hpack2QpackTable `unsafeAt` ix
