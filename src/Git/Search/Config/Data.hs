{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Git.Search.Config.Data
  ( -- * Config
    Config (..),
    RepoConfig (..),

    -- ** Misc
    RepoMapKey (..),

    -- * Command
    Command (..),
    SearchCommitParams (..),
    SearchPullRequestParams (..),
    _DeleteCache,
    _SearchCommit,
    _SearchPullRequest,

    -- ** Env
    DeleteCacheType (..),
    _DeleteCacheGlobal,
    _DeleteCacheLocal,

    -- * Type Families
    AuthF,
    DeleteCacheF,
    RepoF,
    SearchCommitF,
    SearchDefaultsF,
    SearchPullRequestF,
  )
where

import Git.Search.Config.Phase
  ( ConfigF,
    ConfigPhase
      ( ConfigPhaseArgs1,
        ConfigPhaseArgs2,
        ConfigPhaseEnv,
        ConfigPhaseMerged,
        ConfigPhaseToml,
        ConfigPhaseTomlRepoDefaults
      ),
    ConfigWdMaybeF,
  )
import Git.Search.Config.Toml.Utils qualified as Toml.Utils
import Git.Search.Config.WithDisabled (WithDisabled)
import Git.Search.Data
  ( Commit,
    Domain,
    Protocol,
    RepoName,
    RepoPath,
    RepoRemoteName,
    RepoRemoteUri,
  )
import Git.Search.Logging.Data (LogLevel)
import Git.Search.Prelude

type BranchesF :: ConfigPhase -> Type
type family BranchesF p where
  BranchesF ConfigPhaseArgs1 = Maybe (WithDisabled [OsString])
  BranchesF ConfigPhaseToml = [OsString]
  BranchesF ConfigPhaseTomlRepoDefaults = ()
  BranchesF ConfigPhaseMerged = [OsString]
  BranchesF ConfigPhaseEnv = [OsString]

type DomainF :: ConfigPhase -> Type
type family DomainF p where
  DomainF ConfigPhaseArgs1 = Maybe (WithDisabled Domain)
  DomainF ConfigPhaseToml = Maybe Domain
  DomainF ConfigPhaseTomlRepoDefaults = ()
  DomainF ConfigPhaseMerged = Domain
  DomainF ConfigPhaseEnv = ()

type KeyF :: ConfigPhase -> Type
type family KeyF p where
  KeyF ConfigPhaseArgs1 = ()
  KeyF ConfigPhaseToml = Maybe OsString
  KeyF ConfigPhaseTomlRepoDefaults = ()
  KeyF ConfigPhaseMerged = ()
  KeyF ConfigPhaseEnv = ()

type NameF :: ConfigPhase -> Type -> Type
type family NameF p a where
  NameF ConfigPhaseArgs1 a = Maybe (WithDisabled a)
  NameF ConfigPhaseToml a = a
  NameF ConfigPhaseTomlRepoDefaults a = Maybe a
  NameF ConfigPhaseMerged a = Maybe a
  NameF ConfigPhaseEnv _ = ()

type ProtocolF :: ConfigPhase -> Type
type family ProtocolF p where
  ProtocolF ConfigPhaseArgs1 = Maybe (WithDisabled Protocol)
  ProtocolF ConfigPhaseToml = Maybe Protocol
  ProtocolF ConfigPhaseTomlRepoDefaults = ()
  ProtocolF ConfigPhaseMerged = Protocol
  ProtocolF ConfigPhaseEnv = ()

type PathF :: ConfigPhase -> Type
type family PathF p where
  PathF ConfigPhaseArgs1 = Maybe (WithDisabled OsPath)
  PathF ConfigPhaseToml = Maybe OsPath
  PathF ConfigPhaseTomlRepoDefaults = ()
  PathF ConfigPhaseMerged = Maybe OsPath
  PathF ConfigPhaseEnv = ()

-- | Config related to a single repository.
type RepoConfig :: ConfigPhase -> Type
data RepoConfig p = MkRepoConfig
  { -- | Branch filters.
    branches :: BranchesF p,
    -- | Domain e.g. github.com
    domain :: DomainF p,
    -- | Optional key, for allowing multiple map entries with duplicate keys.
    key :: KeyF p,
    -- | Repo name e.g. org/repo
    name :: NameF p RepoName,
    -- | Optional path to repo on the file-system, overrides default cache.
    -- This is OsPath and not RepoPath as we perform the normalization at the
    -- Merged -> Env stage.
    path :: PathF p,
    -- | Protocol e.g. https
    protocol :: ProtocolF p,
    -- | Remote name e.g. 'origin'.
    remoteName :: ConfigWdMaybeF p RepoRemoteName
  }

deriving stock instance
  ( Eq (BranchesF p),
    Eq (DomainF p),
    Eq (KeyF p),
    Eq (NameF p RepoName),
    Eq (PathF p),
    Eq (ProtocolF p),
    Eq (ConfigWdMaybeF p RepoRemoteName)
  ) =>
  Eq (RepoConfig p)

deriving stock instance
  ( Show (BranchesF p),
    Show (DomainF p),
    Show (KeyF p),
    Show (NameF p RepoName),
    Show (PathF p),
    Show (ProtocolF p),
    Show (ConfigWdMaybeF p RepoRemoteName)
  ) =>
  Show (RepoConfig p)

instance DecodeTOML (RepoConfig ConfigPhaseTomlRepoDefaults) where
  tomlDecoder = do
    name <- nameDecoder

    pure
      $ MkRepoConfig
        { branches = (),
          domain = (),
          key = (),
          name,
          path = (),
          protocol = (),
          remoteName = ()
        }
    where
      nameDecoder = getFieldWith tomlDecoder "name"

instance DecodeTOML (RepoConfig ConfigPhaseToml) where
  tomlDecoder = do
    branches <- branchesDecoder
    domain <- domainDecoder
    key <- keyDecoder
    name <- nameDecoder
    path <- pathDecoder
    protocol <- protocolDecoder
    remoteName <- remoteNameDecoder

    pure
      $ MkRepoConfig
        { branches,
          domain,
          key,
          name,
          path,
          protocol,
          remoteName
        }
    where
      branchesDecoder = do
        mBranches <- getFieldOptWith tomlDecoder "branches"
        case mBranches of
          Nothing -> pure []
          Just bs -> traverse encodeFail bs

      domainDecoder = getFieldOptWith tomlDecoder "domain"

      keyDecoder = getFieldOptWith Toml.Utils.osStringDecoder "key"

      nameDecoder = getFieldWith tomlDecoder "name"

      pathDecoder = getFieldOptWith Toml.Utils.osStringDecoder "path"

      protocolDecoder = getFieldOptWith tomlDecoder "protocol"

      remoteNameDecoder = getFieldOptWith tomlDecoder "remote"

-- | Determines what kind of delete we perform.
data DeleteCacheType
  = -- | Delete entire cache.
    DeleteCacheGlobal (Path Abs Dir)
  | -- | Delete specific repo.
    DeleteCacheLocal RepoPath
  deriving stock (Eq, Show)

type DeleteCacheF :: ConfigPhase -> Type
type family DeleteCacheF p where
  DeleteCacheF ConfigPhaseArgs1 = ()
  DeleteCacheF ConfigPhaseArgs2 = ()
  DeleteCacheF ConfigPhaseEnv = DeleteCacheType

type SearchCommitF :: ConfigPhase -> Type
type family SearchCommitF p where
  SearchCommitF ConfigPhaseArgs1 = Commit
  SearchCommitF ConfigPhaseArgs2 = Commit
  SearchCommitF ConfigPhaseEnv = SearchCommitParams

newtype SearchCommitParams = MkSearchCommitParams
  { unSearchCommitParams :: (Commit, RepoPath, RepoRemoteUri)
  }
  deriving stock (Eq, Show)

type SearchDefaultsF :: ConfigPhase -> Type
type family SearchDefaultsF p where
  SearchDefaultsF ConfigPhaseArgs1 = ()
  SearchDefaultsF ConfigPhaseArgs2 = Void
  SearchDefaultsF ConfigPhaseEnv = Void

type SearchPullRequestF :: ConfigPhase -> Type
type family SearchPullRequestF p where
  SearchPullRequestF ConfigPhaseArgs1 = Word32
  SearchPullRequestF ConfigPhaseArgs2 = Word32
  SearchPullRequestF ConfigPhaseEnv = SearchPullRequestParams

newtype SearchPullRequestParams = MkSearchPullRequestParams
  { unSearchPullRequestParams :: (Word32, RepoPath, RepoRemoteUri, RepoName)
  }
  deriving stock (Eq, Show)

-- | Command to run.
type Command :: ConfigPhase -> Type
data Command p
  = -- | Deletes the cache.
    DeleteCache (DeleteCacheF p)
  | -- | Searches for the commit.
    SearchCommit (SearchCommitF p)
  | -- | Runs default searches.
    SearchDefaults (SearchDefaultsF p)
  | -- | Searches for the pull request.
    SearchPullRequest (SearchPullRequestF p)

deriving stock instance
  ( Eq (DeleteCacheF p),
    Eq (SearchCommitF p),
    Eq (SearchDefaultsF p),
    Eq (SearchPullRequestF p)
  ) =>
  Eq (Command p)

deriving stock instance
  ( Show (DeleteCacheF p),
    Show (SearchCommitF p),
    Show (SearchDefaultsF p),
    Show (SearchPullRequestF p)
  ) =>
  Show (Command p)

type AuthF :: ConfigPhase -> Type
type family AuthF p where
  AuthF ConfigPhaseArgs1 = Maybe OsString
  AuthF ConfigPhaseToml = ()
  AuthF ConfigPhaseMerged = Maybe OsString
  AuthF ConfigPhaseEnv = Maybe OsString

-- | Key for toml RepoConfig Map. We have two types as we want it keyed on
-- both the optional field 'key' and mandatory 'name'.
data RepoMapKey
  = -- | Optional 'key' field. This field is entirely user-defined, hence it
    -- allows for multiple entries with the same repo name e.g.
    -- nixos/nixpkgs.
    RepoMapKeyCustom OsString
  | -- | Mandatory 'name' field. This field corresponds to the actual
    -- repository name e.g. nixos/nixpkgs in github.com/nixos/nixpkgs.
    RepoMapKeyName RepoName
  deriving stock (Eq, Show, Ord)

type RepoF :: ConfigPhase -> Type
type family RepoF p where
  RepoF ConfigPhaseArgs1 = RepoConfig ConfigPhaseArgs1
  RepoF ConfigPhaseToml = Map RepoMapKey (RepoConfig ConfigPhaseToml)
  RepoF ConfigPhaseMerged = RepoConfig ConfigPhaseMerged
  RepoF ConfigPhaseEnv = RepoConfig ConfigPhaseEnv

type Config :: ConfigPhase -> Type
data Config p = MkConfig
  { -- | Github auth, for CI.
    auth :: AuthF p,
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

deriving stock instance
  ( Eq (AuthF p),
    Eq (ConfigF p Bool),
    Eq (ConfigWdMaybeF p LogLevel),
    Eq (RepoF p)
  ) =>
  Eq (Config p)

deriving stock instance
  ( Show (AuthF p),
    Show (ConfigF p Bool),
    Show (ConfigWdMaybeF p LogLevel),
    Show (RepoF p)
  ) =>
  Show (Config p)

makeFieldLabelsNoPrefix ''RepoConfig
makeFieldLabelsNoPrefix ''Config
makeFieldLabelsNoPrefix ''SearchCommitParams
makeFieldLabelsNoPrefix ''SearchPullRequestParams
makePrisms ''DeleteCacheType
makePrisms ''Command
