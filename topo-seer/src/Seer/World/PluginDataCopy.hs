{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeApplications #-}

-- | No-follow, handle-relative traversal for plugin-owned save data.
module Seer.World.PluginDataCopy
  ( PluginDataCopyHooks(..)
  , defaultPluginDataCopyHooks
  , copyPluginDataDirectory
  ) where

import Control.Exception (IOException, bracket, onException, try)
import Control.Monad (unless, when)
import Foreign.C.Error (throwErrnoIfMinus1Retry)
import Foreign.C.String (CString)
import Foreign.C.Types (CChar, CInt(..), CUInt(..))
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (IntPtr(..), Ptr, plusPtr)
import System.FilePath
  ( addTrailingPathSeparator
  , dropDrive
  , dropTrailingPathSeparator
  , isAbsolute
  , isPathSeparator
  , splitDirectories
  , takeDrive
  , (</>)
  )
import System.IO.Error (isDoesNotExistError)
#if defined(mingw32_HOST_OS)
import qualified Data.ByteString as BS
import Foreign.C.String (CWString, withCWString)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
#else
import Foreign.C.String (peekCString, withCString)
#endif

-- | Hooks used by deterministic persistence fault/mutation regressions.
-- Production saves use 'defaultPluginDataCopyHooks'.
data PluginDataCopyHooks = PluginDataCopyHooks
  { pdchBeforeEntryOpen :: FilePath -> IO ()
  }

defaultPluginDataCopyHooks :: PluginDataCopyHooks
defaultPluginDataCopyHooks = PluginDataCopyHooks
  { pdchBeforeEntryOpen = const (pure ())
  }

-- | Copy a plugin data root into a relative directory below an existing
-- staging directory. Every source and destination component is opened relative
-- to a pinned parent handle. Symbolic links, Windows reparse points, special
-- source files, and pre-existing destination files are never followed.
-- A missing source root retains the existing "no plugin data yet" behavior.
copyPluginDataDirectory
  :: PluginDataCopyHooks
  -> FilePath       -- ^ Absolute plugin data source
  -> FilePath       -- ^ Existing absolute world staging directory
  -> [FilePath]     -- ^ Validated relative destination directory components
  -> IO ()
copyPluginDataDirectory hooks source stagingPath destinationComponents = do
  openedSource <- try @IOException
    (openExistingAbsoluteDirectory (pdchBeforeEntryOpen hooks) source)
  case openedSource of
    Left err
      | isDoesNotExistError err -> pure ()
      | otherwise -> pluginEntryOpenFailure source err
    Right sourceHandle -> bracket (pure sourceHandle) closeNativeHandle $ \sourceRoot ->
      bracket (openExistingAbsoluteDirectory (const (pure ())) stagingPath) closeNativeHandle $ \stagingRoot ->
        withCreatedDirectoryPath stagingRoot stagingPath destinationComponents $ \destinationRoot destinationPath ->
          copyDirectoryHandle hooks sourceRoot destinationRoot source destinationPath

copyDirectoryHandle
  :: PluginDataCopyHooks
  -> NativeHandle
  -> NativeHandle
  -> FilePath
  -> FilePath
  -> IO ()
copyDirectoryHandle hooks sourceHandle destinationHandle sourcePath destinationPath =
  bracket (openDirectoryEnumeration sourceHandle) closeDirectoryEnumeration loop
  where
    loop enumeration = do
      next <- nextDirectoryEntry enumeration
      case next of
        Nothing -> pure ()
        Just entry -> do
          let childSourcePath = sourcePath </> entry
              childDestinationPath = destinationPath </> entry
          pdchBeforeEntryOpen hooks childSourcePath
          opened <- try @IOException (openChild sourceHandle entry)
          case opened of
            Left err -> pluginEntryOpenFailure childSourcePath err
            Right childHandle -> bracket (pure childHandle) closeNativeHandle $ \child -> do
              childType <- nativeHandleType child
              case childType of
                NativeRegularFile ->
                  bracket (createFileChild destinationHandle entry) closeNativeHandle $ \output ->
                    copyRegularHandle child output
                NativeDirectory ->
                  bracket (createDirectoryChild destinationHandle entry) closeNativeHandle $ \childDestination -> do
                    requireDirectoryHandle childDestination childDestinationPath
                    copyDirectoryHandle hooks child childDestination childSourcePath childDestinationPath
                NativeLink -> symbolicLinkFailure childSourcePath
                NativeSpecial -> specialEntryFailure childSourcePath
          loop enumeration

copyRegularHandle :: NativeHandle -> NativeHandle -> IO ()
copyRegularHandle source destination =
  allocaBytes copyBufferSize $ \buffer -> copyLoop buffer
  where
    copyLoop buffer = do
      count <- throwErrnoIfMinus1Retry "read plugin data entry"
        (c_read_handle source buffer (fromIntegral copyBufferSize))
      when (count > 0) $ do
        writeAll buffer (fromIntegral count)
        copyLoop buffer

    writeAll _ 0 = pure ()
    writeAll buffer remaining = do
      written <- throwErrnoIfMinus1Retry "write staged plugin data entry"
        (c_write_handle destination buffer (fromIntegral remaining))
      when (written == 0) $
        ioError (userError "zero-length write while staging plugin data")
      writeAll (buffer `plusPtr` fromIntegral written) (remaining - fromIntegral written)

copyBufferSize :: Int
copyBufferSize = 64 * 1024

data NativeEntryType
  = NativeRegularFile
  | NativeDirectory
  | NativeLink
  | NativeSpecial

type NativeHandle = IntPtr
type NativeDirectoryEnumeration = IntPtr

#if defined(mingw32_HOST_OS)
foreign import ccall unsafe "topo_plugin_open_root"
  c_open_root :: CWString -> IO IntPtr
#else
foreign import ccall unsafe "topo_plugin_open_root"
  c_open_root :: CString -> IO IntPtr
#endif

foreign import ccall unsafe "topo_plugin_open_child"
  c_open_child :: IntPtr -> CString -> IO IntPtr

foreign import ccall unsafe "topo_plugin_create_directory"
  c_create_directory :: IntPtr -> CString -> IO IntPtr

foreign import ccall unsafe "topo_plugin_create_file"
  c_create_file :: IntPtr -> CString -> IO IntPtr

foreign import ccall unsafe "topo_plugin_dir_enum_open"
  c_dir_enum_open :: IntPtr -> IO IntPtr

foreign import ccall unsafe "topo_plugin_dir_enum_next"
  c_dir_enum_next :: IntPtr -> Ptr CChar -> CUInt -> IO CInt

foreign import ccall unsafe "topo_plugin_dir_enum_close"
  c_dir_enum_close :: IntPtr -> IO ()

foreign import ccall unsafe "topo_plugin_handle_type"
  c_handle_type :: IntPtr -> IO CInt

foreign import ccall safe "topo_plugin_read_handle"
  c_read_handle :: IntPtr -> Ptr a -> CUInt -> IO CInt

foreign import ccall safe "topo_plugin_write_handle"
  c_write_handle :: IntPtr -> Ptr a -> CUInt -> IO CInt

foreign import ccall unsafe "topo_plugin_close_handle"
  c_close_handle :: IntPtr -> IO CInt

openExistingAbsoluteDirectory :: (FilePath -> IO ()) -> FilePath -> IO NativeHandle
openExistingAbsoluteDirectory beforeOpen path = do
  unless (isAbsolute path) $
    ioError (userError ("plugin data handle path is not absolute: " <> path))
  let (anchorPath, components) = absolutePathParts path
  anchor <- openAnchor anchorPath
  requireDirectoryHandle anchor anchorPath `onException` closeNativeHandle anchor
  descend anchor anchorPath components
  where
    descend current _ [] = pure current
    descend current currentPath (component:rest) = do
      let childPath = currentPath </> component
      beforeOpen childPath `onException` closeNativeHandle current
      child <- openChild current component `onException` closeNativeHandle current
      requireDirectoryHandle child childPath `onException` do
        closeNativeHandle child
        closeNativeHandle current
      closeNativeHandle current
      descend child childPath rest

absolutePathParts :: FilePath -> (FilePath, [FilePath])
absolutePathParts path =
  ( addTrailingPathSeparator (takeDrive path)
  , [ component
    | rawComponent <- splitDirectories (dropDrive path)
    , let component = cleanComponent rawComponent
    , not (null component)
    , not (all isPathSeparator component)
    ]
  )
  where
    cleanComponent = dropTrailingPathSeparator

openAnchor :: FilePath -> IO NativeHandle
#if defined(mingw32_HOST_OS)
openAnchor path = withCWString path $ \pathPtr ->
#else
openAnchor path = withCString path $ \pathPtr ->
#endif
  throwErrnoIfMinus1Retry "open filesystem anchor without following links" (c_open_root pathPtr)

openChild :: NativeHandle -> FilePath -> IO NativeHandle
openChild parent name = withNativeChildName name $ \namePtr ->
  throwErrnoIfMinus1Retry "open filesystem entry without following links"
    (c_open_child parent namePtr)

createDirectoryChild :: NativeHandle -> FilePath -> IO NativeHandle
createDirectoryChild parent name = withNativeChildName name $ \namePtr ->
  throwErrnoIfMinus1Retry "create staged plugin data directory without following links"
    (c_create_directory parent namePtr)

createFileChild :: NativeHandle -> FilePath -> IO NativeHandle
createFileChild parent name = withNativeChildName name $ \namePtr ->
  throwErrnoIfMinus1Retry "create staged plugin data file without following links"
    (c_create_file parent namePtr)

withCreatedDirectoryPath
  :: NativeHandle
  -> FilePath
  -> [FilePath]
  -> (NativeHandle -> FilePath -> IO a)
  -> IO a
withCreatedDirectoryPath parent currentPath [] action = action parent currentPath
withCreatedDirectoryPath parent currentPath (component:rest) action = do
  when (null component || component == "." || component == ".." || any isPathSeparator component) $
    ioError (userError ("invalid staged plugin data path component: " <> component))
  let childPath = currentPath </> component
  bracket (createDirectoryChild parent component) closeNativeHandle $ \child -> do
    requireDirectoryHandle child childPath
    withCreatedDirectoryPath child childPath rest action

closeNativeHandle :: NativeHandle -> IO ()
closeNativeHandle handle = do
  -- Never retry close: POSIX permits EINTR after the descriptor was released.
  _ <- c_close_handle handle
  pure ()

openDirectoryEnumeration :: NativeHandle -> IO NativeDirectoryEnumeration
openDirectoryEnumeration handle =
  throwErrnoIfMinus1Retry "open plugin data directory enumeration" (c_dir_enum_open handle)

closeDirectoryEnumeration :: NativeDirectoryEnumeration -> IO ()
closeDirectoryEnumeration = c_dir_enum_close

nextDirectoryEntry :: NativeDirectoryEnumeration -> IO (Maybe FilePath)
nextDirectoryEntry enumeration =
  allocaBytes directoryEntryBufferSize $ \buffer -> do
    result <- throwErrnoIfMinus1Retry "enumerate plugin data directory"
      (c_dir_enum_next enumeration buffer (fromIntegral directoryEntryBufferSize))
    case result of
      0 -> pure Nothing
      1 -> Just <$> peekNativeChildName buffer
      _ -> ioError (userError "invalid plugin data directory enumeration result")

directoryEntryBufferSize :: Int
directoryEntryBufferSize = 64 * 1024

withNativeChildName :: FilePath -> (CString -> IO a) -> IO a
peekNativeChildName :: CString -> IO FilePath
#if defined(mingw32_HOST_OS)
withNativeChildName name action =
  BS.useAsCString (TextEncoding.encodeUtf8 (Text.pack name)) action

peekNativeChildName value = do
  bytes <- BS.packCString value
  case TextEncoding.decodeUtf8' bytes of
    Left err -> ioError (userError ("invalid UTF-8 plugin data entry name: " <> show err))
    Right name -> pure (Text.unpack name)
#else
withNativeChildName = withCString
peekNativeChildName = peekCString
#endif

nativeHandleType :: NativeHandle -> IO NativeEntryType
nativeHandleType handle = do
  result <- throwErrnoIfMinus1Retry "inspect plugin data handle" (c_handle_type handle)
  pure $ case result of
    1 -> NativeRegularFile
    2 -> NativeDirectory
    3 -> NativeLink
    _ -> NativeSpecial

requireDirectoryHandle :: NativeHandle -> FilePath -> IO ()
requireDirectoryHandle handle path = do
  entryType <- nativeHandleType handle
  case entryType of
    NativeDirectory -> pure ()
    NativeLink -> symbolicLinkFailure path
    _ -> specialEntryFailure path

pluginEntryOpenFailure :: FilePath -> IOException -> IO a
pluginEntryOpenFailure path err =
  ioError (userError
    ("failed to open plugin data entry without following symbolic links: "
      <> path <> ": " <> show err))

symbolicLinkFailure :: FilePath -> IO a
symbolicLinkFailure path =
  ioError (userError ("plugin data entry is a symbolic link or reparse point: " <> path))

specialEntryFailure :: FilePath -> IO a
specialEntryFailure path =
  ioError (userError ("plugin data entry is not a regular file or directory: " <> path))
