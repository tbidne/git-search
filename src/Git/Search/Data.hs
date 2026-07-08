module Git.Search.Data
  ( Protocol (..),
    Commit (..),
    Domain (..),
    RepoPath (..),
    RepoRemoteName (..),
    RepoRemoteUri (..),
    RepoName (..),
  )
where

import Git.Search.Config.Toml.Utils
  ( OsStringToml (MkOsStringToml),
    PathAbsDirToml (MkPathAbsDirToml),
  )
import Git.Search.Prelude

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
