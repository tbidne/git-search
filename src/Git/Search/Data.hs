module Git.Search.Data
  ( Protocol (..),
    Commit (..),
    RepoPath (..),
    RepoRemoteUri (..),
    RepoName (..),
  )
where

import Git.Search.Prelude

data Protocol
  = ProtocolHttps
  | ProtocolSsh

instance DecodeTOML Protocol where
  tomlDecoder =
    tomlDecoder @String >>= \case
      "https" -> pure ProtocolHttps
      "ssh" -> pure ProtocolSsh
      other -> fail $ "Unknown protocol: " ++ other

-- | Commit hash.
newtype Commit = MkCommit {unCommit :: OsString}

-- | Repository path on the file-system.
newtype RepoPath = MkRepoPath {unRepoPath :: Path Abs Dir}

-- | Repository remote source i.e. a URL.
newtype RepoRemoteUri = MkRepoRemoteUri {unRepoRemoteUri :: OsString}

-- | Repository name e.g. org/repo.
newtype RepoName = MkRepoName {unRepoName :: OsString}
