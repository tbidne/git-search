module Git.Search.Config.Data
  ( -- * Config
    Config (..),
    RepoConfig (..),

    -- * Command
    Command (..),

    -- ** Env
    DeleteCacheType (..),
  )
where

import Git.Search.Config.Phase
import Git.Search.Config.WithDisabled (WithDisabled)
import Git.Search.Data
  ( Commit,
    Protocol,
    RepoName,
    RepoPath,
    RepoRemoteUri,
  )
import Git.Search.Logging.Data (LogLevel)
import Git.Search.Prelude

type NameF :: ConfigPhase -> Type
type family NameF p where
  NameF ConfigPhaseArgs = Maybe (WithDisabled RepoName)
  NameF ConfigPhaseToml = Maybe RepoName
  NameF ConfigPhaseMerged = Maybe RepoName

type RepoConfig :: ConfigPhase -> Type
data RepoConfig p = MkRepoConfig
  { -- | Domain e.g. github.com
    domain :: ConfigWdF p OsString,
    -- | Repo name e.g. org/repo
    name :: NameF p,
    -- | Protocol e.g. https
    protocol :: ConfigWdF p Protocol
  }

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
  SearchCommitF ConfigPhaseEnv = (Commit, RepoPath, RepoRemoteUri)

type SearchPullRequestF :: ConfigPhase -> Type
type family SearchPullRequestF p where
  SearchPullRequestF ConfigPhaseArgs = Word32
  SearchPullRequestF ConfigPhaseEnv = (Word32, RepoPath, RepoRemoteUri, RepoName)

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
    logLevel :: ConfigWdMaybeF p LogLevel,
    -- | Repo params.
    repo :: RepoF p
  }
