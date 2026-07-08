module Git.Search.Config.Toml.Utils
  ( -- * Newtypes
    OsStringToml (..),
    OsPathValidToml (..),
    PathAbsDirToml (..),

    -- * Decoders
    osStringDecoder,
    osPathDecoder,
  )
where

import FileSystem.Path qualified as FS.Path
import Git.Search.Prelude
import TOML (Decoder)

newtype OsStringToml = MkOsStringToml {unOsStringToml :: OsString}

instance DecodeTOML OsStringToml where
  tomlDecoder = MkOsStringToml <$> osStringDecoder

newtype OsPathValidToml = MkOsPathValidToml {unOsPathValidToml :: OsPath}

instance DecodeTOML OsPathValidToml where
  tomlDecoder = MkOsPathValidToml <$> osPathDecoder

newtype PathAbsDirToml = MkPathAbsDirToml {unPathAbsDirToml :: Path Abs Dir}

instance DecodeTOML PathAbsDirToml where
  tomlDecoder = do
    p <- osPathDecoder
    case FS.Path.parseAbsDir p of
      Nothing -> fail $ "Failed parsing absolute directory: " ++ decodeLenient p
      Just path -> pure $ MkPathAbsDirToml path

osStringDecoder :: Decoder OsString
osStringDecoder = tomlDecoder >>= encodeFail

osPathDecoder :: Decoder OsPath
osPathDecoder = tomlDecoder >>= encodeValidFail
