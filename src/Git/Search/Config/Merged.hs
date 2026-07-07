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
  (HasCallStack) =>
  Args ->
  Maybe Toml ->
  Eff es MergedConfig
mergeConfig args Nothing = do
  name <- mergeRepoNames args.coreConfig.repo.name Nothing

  pure
    $ MkMergedConfig
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
mergeConfig args (Just toml) = do
  name <- mergeRepoNames args.coreConfig.repo.name toml.coreConfig.repo.name

  pure
    $ MkMergedConfig
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
  (HasCallStack) =>
  Maybe OsString ->
  Maybe OsString ->
  Eff es OsString
mergeRepoNames (Just n) _ = pure n
mergeRepoNames Nothing (Just n) = pure n
mergeRepoNames Nothing Nothing =
  throwString "Repository name must be specified by CLI args or Toml config."

mergeBranches ::
  Maybe (WithDisabled [OsString]) ->
  Maybe (Map OsString [OsString]) ->
  OsString ->
  [OsString]
mergeBranches Nothing Nothing _ = []
mergeBranches (Just Disabled) _ _ = []
mergeBranches (Just (With branches)) _ _ = branches
mergeBranches Nothing (Just branchMap) repoName =
  Map.findWithDefault [] repoName branchMap

mkDomain :: Maybe OsString -> OsString
mkDomain = fromMaybe [osstr|github.com|]

mkProtocol :: Maybe Protocol -> Protocol
mkProtocol = fromMaybe ProtocolHttps
