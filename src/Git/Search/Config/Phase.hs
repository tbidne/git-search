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
  = ConfigPhaseArgs
  | ConfigPhaseToml
  | ConfigPhaseMerged
  | ConfigPhaseEnv

type ConfigF :: ConfigPhase -> Type -> Type
type family ConfigF p a where
  ConfigF ConfigPhaseArgs a = Maybe a
  ConfigF ConfigPhaseToml a = Maybe a
  ConfigF ConfigPhaseMerged a = a
  ConfigF ConfigPhaseEnv a = a

type ConfigWdMaybeF :: ConfigPhase -> Type -> Type
type family ConfigWdMaybeF p a where
  ConfigWdMaybeF ConfigPhaseArgs a = Maybe (WithDisabled a)
  ConfigWdMaybeF ConfigPhaseToml a = Maybe a
  ConfigWdMaybeF ConfigPhaseMerged a = Maybe a
  ConfigWdMaybeF ConfigPhaseEnv a = Maybe a
