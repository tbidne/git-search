{-# LANGUAGE QuasiQuotes #-}

module Git.Search.Config.Merged
  ( MergedConfig (..),
    mergeConfig,
  )
where

import Data.Map.Strict qualified as Map
import Git.Search.Config.Args (Args (coreConfig))
import Git.Search.Config.Data
  ( Config (MkConfig, clean, logColor, logLevel, repo),
    RepoConfig (MkRepoConfig, branches, domain, name, protocol),
    RepoMapVal (branches, domain, protocol),
  )
import Git.Search.Config.Phase (ConfigPhase (ConfigPhaseMerged))
import Git.Search.Config.Toml (Toml (coreConfig))
import Git.Search.Config.WithDisabled (WithDisabled (Disabled, With))
import Git.Search.Config.WithDisabled qualified as WD
import Git.Search.Data
  ( Protocol (ProtocolHttps),
    RepoName,
  )
import Git.Search.Prelude

newtype MergedConfig = MkMergedConfig {coreConfig :: Config ConfigPhaseMerged}

mergeConfig ::
  Args ->
  Maybe Toml ->
  MergedConfig
mergeConfig args Nothing =
  let name = mergeMaybe args.coreConfig.repo.name Nothing
   in MkMergedConfig
        { coreConfig =
            MkConfig
              { clean = mergeBoolFalse args.coreConfig.clean Nothing,
                logColor = mergeBoolTrue args.coreConfig.logColor Nothing,
                logLevel =
                  mergeMaybe args.coreConfig.logLevel Nothing,
                repo =
                  MkRepoConfig
                    { branches =
                        mergeBranches
                          args.coreConfig.repo.branches
                          Map.empty
                          name,
                      domain =
                        mergeWD
                          defDomain
                          (.domain)
                          args.coreConfig.repo.domain
                          Map.empty
                          name,
                      name,
                      protocol =
                        mergeWD
                          defProtocol
                          (.protocol)
                          args.coreConfig.repo.protocol
                          Map.empty
                          name
                    }
              }
        }
mergeConfig args (Just toml) =
  let name = mergeMaybe args.coreConfig.repo.name Nothing
   in MkMergedConfig
        { coreConfig =
            MkConfig
              { clean =
                  mergeBoolFalse
                    args.coreConfig.clean
                    toml.coreConfig.clean,
                logColor =
                  mergeBoolTrue
                    args.coreConfig.logColor
                    toml.coreConfig.logColor,
                logLevel =
                  mergeMaybe
                    args.coreConfig.logLevel
                    toml.coreConfig.logLevel,
                repo =
                  MkRepoConfig
                    { branches =
                        mergeBranches
                          args.coreConfig.repo.branches
                          toml.coreConfig.repo
                          name,
                      domain =
                        mergeWD
                          defDomain
                          (.domain)
                          args.coreConfig.repo.domain
                          toml.coreConfig.repo
                          name,
                      name,
                      protocol =
                        mergeWD
                          defProtocol
                          (.protocol)
                          args.coreConfig.repo.protocol
                          toml.coreConfig.repo
                          name
                    }
              }
        }

mergeBoolFalse :: Maybe Bool -> Maybe Bool -> Bool
mergeBoolFalse (Just b) _ = b
mergeBoolFalse Nothing (Just b) = b
mergeBoolFalse Nothing Nothing = False

mergeBoolTrue :: Maybe Bool -> Maybe Bool -> Bool
mergeBoolTrue (Just b) _ = b
mergeBoolTrue Nothing (Just b) = b
mergeBoolTrue Nothing Nothing = True

mergeMaybe :: Maybe (WithDisabled a) -> Maybe a -> Maybe a
mergeMaybe (Just Disabled) _ = Nothing
mergeMaybe a b = WD.toMaybe a <|> b

mergeBranches ::
  Maybe (WithDisabled [OsString]) ->
  Map RepoName RepoMapVal ->
  Maybe RepoName ->
  [OsString]
mergeBranches (Just Disabled) _ _ = []
mergeBranches (Just (With branches)) _ _ = branches
mergeBranches _ _ Nothing = []
mergeBranches Nothing repoMap (Just repoName) =
  maybe [] (.branches) $ Map.lookup repoName repoMap

mergeWD ::
  a ->
  (RepoMapVal -> Maybe a) ->
  Maybe (WithDisabled a) ->
  Map RepoName RepoMapVal ->
  Maybe RepoName ->
  a
mergeWD defA _ (Just Disabled) _ _ = defA
mergeWD _ _ (Just (With x)) _ _ = x
mergeWD defA _ Nothing _ Nothing = defA
mergeWD defA toA Nothing repoMap (Just repoName) =
  case Map.lookup repoName repoMap of
    Nothing -> defA
    Just rmv -> fromMaybe defA (toA rmv)

defDomain :: OsString
defDomain = [osstr|github.com|]

defProtocol :: Protocol
defProtocol = ProtocolHttps
