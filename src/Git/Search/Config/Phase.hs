module Git.Search.Config.Phase
  ( -- * Phases
    ConfigPhase (..),

    -- * Type families
    ConfigF,
    ConfigWdMaybeF,
  )
where

import Git.Search.Config.WithDisabled (WithDisabled)
import Git.Search.Prelude

data ConfigPhase
  = -- | CLI Args phase.
    ConfigPhaseArgs1
  | -- | CLI Args second phase. Used after initial pattern-match so that we
    -- can eliminate SearchDefaults constructor at compile-time.
    ConfigPhaseArgs2
  | -- | Toml phase.
    ConfigPhaseToml
  | -- | Toml phased, used only for repo-defaults field.
    ConfigPhaseTomlRepoDefaults
  | -- | Merged phase.
    ConfigPhaseMerged
  | -- | Runtime env phase.
    ConfigPhaseEnv

type ConfigF :: ConfigPhase -> Type -> Type
type family ConfigF p a where
  ConfigF ConfigPhaseArgs1 a = Maybe a
  ConfigF ConfigPhaseToml a = Maybe a
  ConfigF ConfigPhaseMerged a = a
  ConfigF ConfigPhaseEnv a = a

type ConfigWdMaybeF :: ConfigPhase -> Type -> Type
type family ConfigWdMaybeF p a where
  ConfigWdMaybeF ConfigPhaseArgs1 a = Maybe (WithDisabled a)
  ConfigWdMaybeF ConfigPhaseToml a = Maybe a
  ConfigWdMaybeF ConfigPhaseTomlRepoDefaults _ = ()
  ConfigWdMaybeF ConfigPhaseMerged a = Maybe a
  ConfigWdMaybeF ConfigPhaseEnv a = Maybe a
