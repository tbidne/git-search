module Git.Search.Config.Data
  ( -- * Config
    Config (..),
    RepoConfig (..),
    RepoName (..),
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
import Git.Search.Logging.Data (LogLevel)
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

newtype RepoName = MkRepoName {unRepoName :: OsString}

type NameF :: ConfigPhase -> Type
type family NameF p where
  NameF ConfigPhaseArgs = Maybe (WithDisabled RepoName)
  NameF ConfigPhaseToml = Maybe RepoName
  NameF ConfigPhaseMerged = Maybe RepoName

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

type SearchPullRequestF :: ConfigPhase -> Type
type family SearchPullRequestF p where
  SearchPullRequestF ConfigPhaseArgs = Word32
  SearchPullRequestF ConfigPhaseEnv = (Word32, RepoPath, RepoSrc, RepoName)

-- | Command to run.
type Command :: ConfigPhase -> Type
data Command p
  = -- | Deletes the cache.
    DeleteCache (DeleteCacheF p)
  | -- | Searches for the commit.
    SearchCommit (SearchCommitF p)
  | -- | Searches for the pull request.
    SearchPullRequest (SearchPullRequestF p)

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

type ConfigMaybeF :: ConfigPhase -> Type -> Type
type family ConfigMaybeF p a where
  ConfigMaybeF ConfigPhaseArgs a = Maybe (WithDisabled a)
  ConfigMaybeF ConfigPhaseToml a = Maybe a
  ConfigMaybeF ConfigPhaseMerged a = Maybe a
  ConfigMaybeF ConfigPhaseEnv a = Maybe a

type Config :: ConfigPhase -> Type
data Config p = MkConfig
  { -- | Branch filters.
    branches :: BranchesF p,
    -- | Performs a clean clone of the repo. Otherwise runs 'fetch' if the
    -- repo exists.
    clean :: ConfigF p Bool,
    -- | Log colors.
    logColor :: ConfigF p Bool,
    -- | Logging.
    logLevel :: ConfigMaybeF p LogLevel,
    -- | Repo params.
    repo :: RepoF p
  }
