{-# LANGUAGE QuasiQuotes #-}

module Git.Search.Config.Merged
  ( MergedConfig (..),
    mergeConfig,
  )
where

import Data.Map.Strict qualified as Map
import Git.Search.Config.Args (Args (coreConfig))
import Git.Search.Config.Data
  ( Config (MkConfig, branches, clean, logColor, logLevel, repo),
    ConfigPhase (ConfigPhaseMerged),
    Protocol (ProtocolHttps),
    RepoConfig (MkRepoConfig, domain, name, protocol),
  )
import Git.Search.Config.Toml (Toml (coreConfig))
import Git.Search.Config.WithDisabled (WithDisabled (Disabled, With))
import Git.Search.Config.WithDisabled qualified as WD
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
              { branches =
                  mergeBranches
                    args.coreConfig.branches
                    Nothing
                    name,
                clean = mergeBoolFalse args.coreConfig.clean Nothing,
                logColor = mergeBoolTrue args.coreConfig.logColor Nothing,
                logLevel =
                  mergeMaybe args.coreConfig.logLevel Nothing,
                repo =
                  MkRepoConfig
                    { domain =
                        mergeWD
                          mkDomain
                          args.coreConfig.repo.domain
                          Nothing,
                      name,
                      protocol =
                        mergeWD
                          mkProtocol
                          args.coreConfig.repo.protocol
                          Nothing
                    }
              }
        }
mergeConfig args (Just toml) =
  let name = mergeMaybe args.coreConfig.repo.name toml.coreConfig.repo.name
   in MkMergedConfig
        { coreConfig =
            MkConfig
              { branches =
                  mergeBranches
                    args.coreConfig.branches
                    toml.coreConfig.branches
                    name,
                clean =
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
                    { domain =
                        mergeWD
                          mkDomain
                          args.coreConfig.repo.domain
                          toml.coreConfig.repo.domain,
                      name,
                      protocol =
                        mergeWD
                          mkProtocol
                          args.coreConfig.repo.protocol
                          toml.coreConfig.repo.protocol
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
  Maybe (Map OsString [OsString]) ->
  Maybe OsString ->
  [OsString]
mergeBranches Nothing Nothing _ = []
mergeBranches (Just Disabled) _ _ = []
mergeBranches (Just (With branches)) _ _ = branches
mergeBranches _ _ Nothing = []
mergeBranches Nothing (Just branchMap) (Just repoName) =
  Map.findWithDefault [] repoName branchMap

mergeWD ::
  (Maybe a -> a) ->
  Maybe (WithDisabled a) ->
  Maybe a ->
  a
mergeWD f (Just Disabled) _ = f Nothing
mergeWD f a b = f $ WD.toMaybe a <|> b

mkDomain :: Maybe OsString -> OsString
mkDomain = fromMaybe [osstr|github.com|]

mkProtocol :: Maybe Protocol -> Protocol
mkProtocol = fromMaybe ProtocolHttps
