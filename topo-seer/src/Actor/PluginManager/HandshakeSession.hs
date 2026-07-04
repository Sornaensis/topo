{-# LANGUAGE TypeApplications #-}

-- | Handshake timeout handling for newly launched plugin sessions.
module Actor.PluginManager.HandshakeSession
  ( ExpectedHandshakeCredentials(..)
  , PluginHandshakeError(..)
  , pluginHandshakeTimeoutMicros
  , performPluginHandshakeWithTimeout
  ) where

import Control.Exception (SomeAsyncException, SomeException, fromException, throwIO, try)
import qualified Data.ByteString as BS
import Crypto.Random (getRandomBytes)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word8)
import Numeric (showHex)
import System.Timeout (timeout)

import Topo.Plugin.RPC
  ( HandshakeAuthChallenge(..)
  , RPCConnection
  , RPCError
  , handshakeAuthProof
  , performHandshakeWithAuth
  )

data ExpectedHandshakeCredentials = ExpectedHandshakeCredentials
  { ehcSessionId :: !Text
  , ehcAuthToken :: !Text
  } deriving (Eq)

data PluginHandshakeError
  = PluginHandshakeRPC !RPCError
  | PluginHandshakeException !Text
  deriving (Eq, Show)

pluginHandshakeTimeoutMicros :: Int
pluginHandshakeTimeoutMicros = 1000000

performPluginHandshakeWithTimeout
  :: Int
  -> Maybe ExpectedHandshakeCredentials
  -> RPCConnection
  -> IO (Maybe (Either PluginHandshakeError RPCConnection))
performPluginHandshakeWithTimeout timeoutMicros mCredentials conn = do
  mAuthChallenge <- traverse makeAuthChallenge mCredentials
  result <- timeout (max 1 timeoutMicros) (trySync (performHandshakeWithAuth conn Nothing mAuthChallenge))
  pure $ case result of
    Nothing -> Nothing
    Just (Left err) -> Just (Left (PluginHandshakeException (Text.pack (show err))))
    Just (Right (Left rpcErr)) -> Just (Left (PluginHandshakeRPC rpcErr))
    Just (Right (Right conn')) -> Just (Right conn')

makeAuthChallenge :: ExpectedHandshakeCredentials -> IO HandshakeAuthChallenge
makeAuthChallenge credentials = do
  challenge <- freshChallenge
  pure HandshakeAuthChallenge
    { hacSessionId = ehcSessionId credentials
    , hacChallenge = challenge
    , hacExpectedProof = handshakeAuthProof (ehcSessionId credentials) (ehcAuthToken credentials) challenge
    }

freshChallenge :: IO Text
freshChallenge = do
  bytes <- getRandomBytes 32 :: IO BS.ByteString
  pure (Text.pack ("challenge-" <> bytesToHex bytes))

bytesToHex :: BS.ByteString -> String
bytesToHex = concatMap byteToHex . BS.unpack

byteToHex :: Word8 -> String
byteToHex byte = case showHex byte "" of
  [digit] -> ['0', digit]
  digits  -> digits

trySync :: IO a -> IO (Either SomeException a)
trySync action = do
  result <- try @SomeException action
  case result of
    Left err
      | Just asyncErr <- fromException @SomeAsyncException err -> throwIO asyncErr
      | otherwise -> pure (Left err)
    Right value -> pure (Right value)
