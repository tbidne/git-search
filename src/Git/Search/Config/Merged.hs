{-# LANGUAGE QuasiQuotes #-}

module Git.Search.Config.Merged
  ( MergedConfig (..),
    mergeConfig,
  )
where

import Data.Map.Strict qualified as Map
import Git.Search.Config.Args (Args (coreConfig))
import Git.Search.Config.Data
  ( Config (MkConfig, branches, clean, debug, repo),
    ConfigPhase (ConfigPhaseMerged),
    Protocol (ProtocolHttps),
    RepoConfig (MkRepoConfig, domain, name, protocol),
    WithDisabled (Disabled, With),
  )
import Git.Search.Config.Toml (Toml (coreConfig))
import Git.Search.Prelude

newtype MergedConfig = MkMergedConfig {coreConfig :: Config ConfigPhaseMerged}

mergeConfig ::
  Args ->
  Maybe Toml ->
  MergedConfig
mergeConfig args Nothing =
  let name = mergeRepoNames args.coreConfig.repo.name Nothing
   in MkMergedConfig
        { coreConfig =
            MkConfig
              { branches =
                  mergeBranches
                    args.coreConfig.branches
                    Nothing
                    name,
                clean = mergeBoolFalse args.coreConfig.clean Nothing,
                debug = mergeBoolFalse args.coreConfig.debug Nothing,
                repo =
                  MkRepoConfig
                    { domain = mkDomain args.coreConfig.repo.domain,
                      name,
                      protocol = mkProtocol args.coreConfig.repo.protocol
                    }
              }
        }
mergeConfig args (Just toml) =
  let name = mergeRepoNames args.coreConfig.repo.name toml.coreConfig.repo.name
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
                debug =
                  mergeBoolFalse
                    args.coreConfig.debug
                    toml.coreConfig.debug,
                repo =
                  MkRepoConfig
                    { domain =
                        mkDomain
                          $ args.coreConfig.repo.domain
                          <|> toml.coreConfig.repo.domain,
                      name,
                      protocol =
                        mkProtocol
                          $ args.coreConfig.repo.protocol
                          <|> toml.coreConfig.repo.protocol
                    }
              }
        }

mergeBoolFalse :: Maybe Bool -> Maybe Bool -> Bool
mergeBoolFalse (Just b) _ = b
mergeBoolFalse Nothing (Just b) = b
mergeBoolFalse Nothing Nothing = False

mergeRepoNames ::
  Maybe OsString ->
  Maybe OsString ->
  Maybe OsString
mergeRepoNames m n = m <|> n

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

mkDomain :: Maybe OsString -> OsString
mkDomain = fromMaybe [osstr|github.com|]

mkProtocol :: Maybe Protocol -> Protocol
mkProtocol = fromMaybe ProtocolHttps
