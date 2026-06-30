-- | Handshake timeout handling for newly launched plugin sessions.
module Actor.PluginManager.HandshakeSession
  ( PluginHandshakeError(..)
  , pluginHandshakeTimeoutMicros
  , performPluginHandshakeWithTimeout
  ) where

import Control.Exception (SomeException, try)
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

performPluginHandshakeWithTimeout :: Int -> RPCConnection -> IO (Maybe (Either PluginHandshakeError RPCConnection))
performPluginHandshakeWithTimeout timeoutMicros conn = do
  result <- timeout (max 1 timeoutMicros) (try (performHandshake conn Nothing))
  pure $ case result of
    Nothing -> Nothing
    Just (Left err) -> Just (Left (PluginHandshakeException (Text.pack (show (err :: SomeException)))))
    Just (Right (Left rpcErr)) -> Just (Left (PluginHandshakeRPC rpcErr))
    Just (Right (Right conn')) -> Just (Right conn')
