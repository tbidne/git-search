module Git.Search.Config.Data
  ( -- * Config
    Config (..),
    RepoConfig (..),
    Protocol (..),

    -- ** Env
    RepoEnv (..),

    -- * Phases
    ConfigPhase (..),

    -- * Disabling
    WithDisabled (..),
    disabledParser,
  )
where

import Git.Search.Prelude

data WithDisabled a
  = -- | The field.
    With a
  | -- | Disabled.
    Disabled
  deriving stock (Eq, Functor, Show)

instance (DecodeTOML a) => DecodeTOML (WithDisabled a) where
  tomlDecoder = parseText <|> With <$> tomlDecoder
    where
      parseText = do
        tomlDecoder @Text >>= \case
          "off" -> pure Disabled
          other -> fail $ "Expected 'off', received: " <> unpack other

disabledParser :: (Applicative f) => Text -> f a -> f (WithDisabled a)
disabledParser "off" _ = pure Disabled
disabledParser _ fx = With <$> fx

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
  RepoConfigF ConfigPhaseArgs a = Maybe a
  RepoConfigF ConfigPhaseToml a = Maybe a
  RepoConfigF ConfigPhaseMerged a = a

type RepoConfig :: ConfigPhase -> Type
data RepoConfig p = MkRepoConfig
  { -- | Domain e.g. github.com
    domain :: RepoConfigF p OsString,
    -- | Repo name e.g. org/repo
    name :: RepoConfigF p OsString,
    -- | Protocol e.g. https
    protocol :: RepoConfigF p Protocol
  }

data RepoEnv = MkRepoEnv
  { -- | Path to cloned repo e.g. ~/.cache/git-search/org/repo
    path :: Path Abs Dir,
    -- | Full source e.g. https://github.com/org/repo
    src :: OsString
  }

type RepoF :: ConfigPhase -> Type
type family RepoF p where
  RepoF ConfigPhaseArgs = RepoConfig ConfigPhaseArgs
  RepoF ConfigPhaseToml = RepoConfig ConfigPhaseToml
  RepoF ConfigPhaseMerged = RepoConfig ConfigPhaseMerged
  RepoF ConfigPhaseEnv = RepoEnv

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
  { branches :: BranchesF p,
    -- | Performs a clean clone of the repo. Otherwise runs 'fetch' if the
    -- repo exists.
    clean :: ConfigF p Bool,
    -- | Commit hash to search.
    commit :: ConfigF p OsString,
    -- | Additional debug logging.
    debug :: ConfigF p Bool,
    -- | Repo params.
    repo :: RepoF p
  }
