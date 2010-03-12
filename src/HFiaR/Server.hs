-- | This module provides a Server to run HFiaR actions in a separate eprocess
module HFiaR.Server (
-- * Types
        ServerHandle,
-- * Functions
        start, stop, runIn
    ) where

import Control.Monad
import Control.Monad.Trans
import Control.Concurrent.Process
import HFiaR

-- | The server handle.  It's returned on process creation and should be used
-- afterwards to send messages to it
newtype ServerHandle = SH {handle :: Handle (HFiaR ())}

-- | Starts the server. Usage:
-- @
--      handle <- start
-- @
start :: IO ServerHandle
start = (spawn $ makeProcess play game) >>= return . SH
    where game = forever $ recv >>= lift

-- | Runs the action. Usage:
-- @
--      result <- runIn serverhandle action
-- @
runIn :: ServerHandle   -- ^ The handle of the server that will run the action
       -> HFiaR a       -- ^ The action to be run
       -> IO a
runIn server action = runHere $ do
                                    me <- self
                                    sendTo (handle server) $ action >>= sendTo me
                                    recv

-- | Stops the server. Usage:
-- @
--      stop serverhandle
-- @
stop :: ServerHandle -> IO ()
stop = kill . handle