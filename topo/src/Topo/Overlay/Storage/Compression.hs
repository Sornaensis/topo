{-# LANGUAGE StrictData #-}

-- | Compression options for @.topolay@ chunk payloads.
module Topo.Overlay.Storage.Compression
  ( OverlayCompression(..)
  , OverlayStorageOptions(..)
  , defaultOverlayStorageOptions
  ) where

-- | Available chunk compression modes.
data OverlayCompression
  -- | Disable compression and store raw chunk payload bytes.
  = CompressionNone
  -- | Compress chunk payloads using zstd single-frame encoding.
  | CompressionZstd
  deriving (Eq, Show)

-- | Writer options controlling how overlay chunk payloads are encoded.
data OverlayStorageOptions = OverlayStorageOptions
  -- | Compression mode used for all chunk payloads in a file.
  { osoCompression :: !OverlayCompression
  -- | Requested zstd compression level when 'osoCompression' is
  -- 'CompressionZstd'.
  , osoZstdLevel :: !Int
  } deriving (Eq, Show)

-- | Default storage options: uncompressed payloads, zstd level 3.
defaultOverlayStorageOptions :: OverlayStorageOptions
defaultOverlayStorageOptions = OverlayStorageOptions
  { osoCompression = CompressionNone
  , osoZstdLevel = 3
  }
