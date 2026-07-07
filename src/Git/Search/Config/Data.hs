module Git.Search.Config.Data
  ( -- * Config
    Config (..),
    RepoConfig (..),
    Protocol (..),
    Commit (..),

    -- * Command
    Command (..),

    -- ** Env
    RepoPath (..),
    RepoSrc (..),
    DeleteCacheType (..),

    -- * Phases
    ConfigPhase (..),
  )
where

import Git.Search.Config.WithDisabled
import Git.Search.Prelude

data ConfigPhase
  = ConfigPhaseArgs
  | ConfigPhaseToml
  | ConfigPhaseMerged
  | ConfigPhaseEnv

data Protocol
  = ProtocolHttps
  | ProtocolSsh

instance DecodeTOML Protocol where
  tomlDecoder =
    tomlDecoder @String >>= \case
      "https" -> pure ProtocolHttps
      "ssh" -> pure ProtocolSsh
      other -> fail $ "Unknown protocol: " ++ other

type RepoConfigF :: ConfigPhase -> Type -> Type
type family RepoConfigF p a where
  RepoConfigF ConfigPhaseArgs a = Maybe (WithDisabled a)
  RepoConfigF ConfigPhaseToml a = Maybe a
  RepoConfigF ConfigPhaseMerged a = a

type NameF :: ConfigPhase -> Type
type family NameF p where
  NameF ConfigPhaseArgs = Maybe (WithDisabled OsString)
  NameF ConfigPhaseToml = Maybe OsString
  NameF ConfigPhaseMerged = Maybe OsString

type RepoConfig :: ConfigPhase -> Type
data RepoConfig p = MkRepoConfig
  { -- | Domain e.g. github.com
    domain :: RepoConfigF p OsString,
    -- | Repo name e.g. org/repo
    name :: NameF p,
    -- | Protocol e.g. https
    protocol :: RepoConfigF p Protocol
  }

-- | Commit hash.
newtype Commit = MkCommit {unCommit :: OsString}

-- | Repository path on the file-system.
newtype RepoPath = MkRepoPath {unRepoPath :: Path Abs Dir}

-- | Repository remote source i.e. a URL.
newtype RepoSrc = MkRepoSrc {unRepoSrc :: OsString}

-- | Determines what kind of delete we perform.
data DeleteCacheType
  = -- | Delete entire cache.
    DeleteCacheGlobal (Path Abs Dir)
  | -- | Delete specific repo.
    DeleteCacheLocal RepoPath

type DeleteCacheF :: ConfigPhase -> Type
type family DeleteCacheF p where
  DeleteCacheF ConfigPhaseArgs = ()
  DeleteCacheF ConfigPhaseEnv = DeleteCacheType

type SearchCommitF :: ConfigPhase -> Type
type family SearchCommitF p where
  SearchCommitF ConfigPhaseArgs = Commit
  SearchCommitF ConfigPhaseEnv = (Commit, RepoPath, RepoSrc)

-- | Command to run.
type Command :: ConfigPhase -> Type
data Command p
  = -- | Deletes the cache.
    DeleteCache (DeleteCacheF p)
  | -- | Searches for the commit.
    SearchCommit (SearchCommitF p)

type RepoF :: ConfigPhase -> Type
type family RepoF p where
  RepoF ConfigPhaseArgs = RepoConfig ConfigPhaseArgs
  RepoF ConfigPhaseToml = RepoConfig ConfigPhaseToml
  RepoF ConfigPhaseMerged = RepoConfig ConfigPhaseMerged
  RepoF ConfigPhaseEnv = ()

type BranchesF :: ConfigPhase -> Type
type family BranchesF p where
  BranchesF ConfigPhaseArgs = Maybe (WithDisabled [OsString])
  BranchesF ConfigPhaseToml = Maybe (Map OsString [OsString])
  BranchesF ConfigPhaseMerged = [OsString]
  BranchesF ConfigPhaseEnv = [OsString]

type ConfigF :: ConfigPhase -> Type -> Type
type family ConfigF p a where
  ConfigF ConfigPhaseArgs a = Maybe a
  ConfigF ConfigPhaseToml a = Maybe a
  ConfigF ConfigPhaseMerged a = a
  ConfigF ConfigPhaseEnv a = a

type Config :: ConfigPhase -> Type
data Config p = MkConfig
  { -- | Branch filters.
    branches :: BranchesF p,
    -- | Performs a clean clone of the repo. Otherwise runs 'fetch' if the
    -- repo exists.
    clean :: ConfigF p Bool,
    -- | Additional debug logging.
    debug :: ConfigF p Bool,
    -- | Repo params.
    repo :: RepoF p
  }
