module Looper where

import qualified Foreign.Ptr as Foreign
import Foreign.StablePtr

foreign export ccall hs_looper_init :: IO (StablePtr LooperState)
foreign export ccall hs_looper_main 
    :: StablePtr LooperState
    -> Int -- window size
    -> Int -- input channels
    -> Int -- output channels
    -> Foreign.Ptr (Foreign.Ptr Float) -- channel data
    -> IO ()
foreign export ccall hs_looper_exit :: StablePtr LooperState -> IO ()

data LooperState = LooperState Int

hs_looper_init = newStablePtr (LooperState 0)
hs_looper_main state window input output channels = return ()
hs_looper_exit state = freeStablePtr state
