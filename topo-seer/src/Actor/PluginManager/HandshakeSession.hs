-- | Handshake timeout handling for newly launched plugin sessions.
module Actor.PluginManager.HandshakeSession
  ( PluginHandshakeError(..)
  , pluginHandshakeTimeoutMicros
  , performPluginHandshakeWithTimeout
  ) where

import Control.Concurrent (forkFinally, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Timeout (timeout)

import Topo.Plugin.RPC (RPCConnection, RPCError, performHandshake)

data PluginHandshakeError
  = PluginHandshakeRPC !RPCError
  | PluginHandshakeException !Text
  deriving (Eq, Show)

pluginHandshakeTimeoutMicros :: Int
pluginHandshakeTimeoutMicros = 1000000

performPluginHandshakeWithTimeout :: RPCConnection -> IO (Maybe (Either PluginHandshakeError RPCConnection))
performPluginHandshakeWithTimeout conn = do
  done <- newEmptyMVar
  _ <- forkFinally (performHandshake conn Nothing) $ \result ->
    putMVar done $ case result of
      Left err -> Left (PluginHandshakeException (Text.pack (show (err :: SomeException))))
      Right (Left rpcErr) -> Left (PluginHandshakeRPC rpcErr)
      Right (Right conn') -> Right conn'
  timeout pluginHandshakeTimeoutMicros (takeMVar done)
