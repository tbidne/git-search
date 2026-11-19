{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Git.Search.Data
  ( Protocol (..),
    _ProtocolHttps,
    _ProtocolSsh,
    Branch (..),
    Commit (..),
    Domain (..),
    RepoPath (..),
    RepoRemoteName (..),
    RepoRemoteUri (..),
    RepoName (..),
    RepoUpdate (..),
  )
where

import Git.Search.Config.Toml.Utils
  ( OsStringToml (MkOsStringToml),
    PathAbsDirToml (MkPathAbsDirToml),
  )
import Git.Search.Prelude

-- | Determines repo update behavior.
data RepoUpdate
  = -- | Fetches if the repo exists, otherwise clones.
    RepoUpdateAuto
  | -- | Cleans the repo i.e. always clones.
    RepoUpdateClean
  | -- | Skips clone/fetch if the repo exists, otherwise cleans.
    RepoUpdateOff
  deriving stock (Eq, Show)

instance DecodeTOML RepoUpdate where
  tomlDecoder =
    tomlDecoder @String >>= \case
      "auto" -> pure RepoUpdateAuto
      "clean" -> pure RepoUpdateClean
      "off" -> pure RepoUpdateOff
      other -> fail $ "Unrecognized update: " ++ other

data Protocol
  = ProtocolHttps
  | ProtocolSsh
  deriving stock (Eq, Ord, Show)

instance DecodeTOML Protocol where
  tomlDecoder =
    tomlDecoder @String >>= \case
      "https" -> pure ProtocolHttps
      "ssh" -> pure ProtocolSsh
      other -> fail $ "Unknown protocol: " ++ other

-- | Branch name.
newtype Branch = MkBranch {unBranch :: OsString}
  deriving stock (Eq, Ord, Show)
  deriving (DecodeTOML) via OsStringToml

-- | Commit hash.
newtype Commit = MkCommit {unCommit :: OsString}
  deriving stock (Eq, Ord, Show)
  deriving (DecodeTOML) via OsStringToml

-- | Repository domain
newtype Domain = MkDomain {unDomain :: OsString}
  deriving stock (Eq, Ord, Show)
  deriving (DecodeTOML) via OsStringToml

-- | Repository path on the file-system.
newtype RepoPath = MkRepoPath {unRepoPath :: Path Abs Dir}
  deriving stock (Eq, Ord, Show)
  deriving (DecodeTOML) via PathAbsDirToml

-- | Repository remote name e.g. 'origin'.
newtype RepoRemoteName = MkRepoRemoteName {unRepoRemoteName :: OsString}
  deriving stock (Eq, Ord, Show)
  deriving (DecodeTOML) via OsStringToml

-- | Repository remote source i.e. a URL.
newtype RepoRemoteUri = MkRepoRemoteUri {unRepoRemoteUri :: OsString}
  deriving stock (Eq, Ord, Show)
  deriving (DecodeTOML) via OsStringToml

-- | Repository name e.g. org/repo.
newtype RepoName = MkRepoName {unRepoName :: OsString}
  deriving stock (Eq, Ord, Show)
  deriving (DecodeTOML) via OsStringToml

makePrisms ''Protocol
makeFieldLabelsNoPrefix ''Branch
makeFieldLabelsNoPrefix ''Commit
makeFieldLabelsNoPrefix ''Domain
makeFieldLabelsNoPrefix ''RepoPath
makeFieldLabelsNoPrefix ''RepoRemoteName
makeFieldLabelsNoPrefix ''RepoRemoteUri
makeFieldLabelsNoPrefix ''RepoName
