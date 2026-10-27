{-# LANGUAGE QuasiQuotes #-}

module Git.Search.Config.Merged
  ( MergedConfig (..),
    mergeConfig,
  )
where

import Data.Map.Strict qualified as Map
import Git.Search.Config.Args (Args (coreConfig))
import Git.Search.Config.Data
  ( Config (MkConfig, auth, clean, logColor, logLevel, repo),
    RepoConfig
      ( MkRepoConfig,
        branches,
        domain,
        key,
        name,
        path,
        protocol,
        remoteName
      ),
    RepoMapKey (RepoMapKeyCustom, RepoMapKeyName),
  )
import Git.Search.Config.Phase
  ( ConfigPhase
      ( ConfigPhaseArgs2,
        ConfigPhaseMerged,
        ConfigPhaseToml
      ),
  )
import Git.Search.Config.Toml (Toml (coreConfig))
import Git.Search.Config.WithDisabled (WithDisabled (Disabled, With))
import Git.Search.Config.WithDisabled qualified as WD
import Git.Search.Data
  ( Domain (MkDomain),
    Protocol (ProtocolHttps),
    RepoName (unRepoName),
  )
import Git.Search.Prelude

newtype MergedConfig = MkMergedConfig {coreConfig :: Config ConfigPhaseMerged}

mergeConfig ::
  Args ConfigPhaseArgs2 ->
  Maybe Toml ->
  MergedConfig
mergeConfig args mToml = case mToml of
  Nothing ->
    MkMergedConfig
      { coreConfig =
          MkConfig
            { auth = args.coreConfig.auth,
              clean = mergeBoolFalse args.coreConfig.clean Nothing,
              logColor = mergeBoolTrue args.coreConfig.logColor Nothing,
              logLevel =
                mergeMaybe args.coreConfig.logLevel Nothing,
              repo =
                MkRepoConfig
                  { branches =
                      mergeBranches
                        args.coreConfig.repo.branches
                        Nothing,
                    domain =
                      mergeWD
                        defDomain
                        (.domain)
                        args.coreConfig.repo.domain
                        Nothing,
                    key = (),
                    name,
                    path =
                      mergeWDMaybe
                        (.path)
                        args.coreConfig.repo.path
                        Nothing,
                    protocol =
                      mergeWD
                        defProtocol
                        (.protocol)
                        args.coreConfig.repo.protocol
                        Nothing,
                    remoteName =
                      mergeWDMaybe
                        (.remoteName)
                        args.coreConfig.repo.remoteName
                        Nothing
                  }
            }
      }
  Just toml ->
    MkMergedConfig
      { coreConfig =
          MkConfig
            { auth = args.coreConfig.auth,
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
                  { branches =
                      mergeBranches
                        args.coreConfig.repo.branches
                        mRepo,
                    domain =
                      mergeWD
                        defDomain
                        (.domain)
                        args.coreConfig.repo.domain
                        mRepo,
                    key = (),
                    name,
                    path =
                      mergeWDMaybe
                        (.path)
                        args.coreConfig.repo.path
                        mRepo,
                    protocol =
                      mergeWD
                        defProtocol
                        (.protocol)
                        args.coreConfig.repo.protocol
                        mRepo,
                    remoteName =
                      mergeWDMaybe
                        (.remoteName)
                        args.coreConfig.repo.remoteName
                        mRepo
                  }
            }
      }
  where
    -- initName is either the CLI name or toml default name.
    initName =
      mergeMaybe
        args.coreConfig.repo.name
        (mToml ^? _Just % #repoDefaults % #name % _Just)

    (name, mRepo) = case initName of
      Nothing -> (Nothing, Nothing)
      Just n -> case (mToml ^? _Just % #coreConfig % #repo) >>= repoLookup n of
        Nothing -> (Just n, Nothing)
        Just r -> (Just r.name, Just r)

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
  Maybe (RepoConfig ConfigPhaseToml) ->
  [OsString]
mergeBranches (Just Disabled) _ = []
mergeBranches (Just (With branches)) _ = branches
mergeBranches Nothing mRepo = maybe [] (.branches) mRepo

mergeWDMaybe ::
  (RepoConfig ConfigPhaseToml -> Maybe a) ->
  Maybe (WithDisabled a) ->
  Maybe (RepoConfig ConfigPhaseToml) ->
  Maybe a
mergeWDMaybe _ (Just Disabled) _ = Nothing
mergeWDMaybe _ (Just (With x)) _ = Just x
mergeWDMaybe toA Nothing mRepo = toA =<< mRepo

mergeWD ::
  a ->
  (RepoConfig ConfigPhaseToml -> Maybe a) ->
  Maybe (WithDisabled a) ->
  Maybe (RepoConfig ConfigPhaseToml) ->
  a
mergeWD defA _ (Just Disabled) _ = defA
mergeWD _ _ (Just (With x)) _ = x
mergeWD defA toA Nothing mRepo = fromMaybe defA (mRepo >>= toA)

defDomain :: Domain
defDomain = MkDomain [osstr|github.com|]

defProtocol :: Protocol
defProtocol = ProtocolHttps

-- | Performs a lookup in the map. First attempts a 'key' match, then a
-- 'name' one.
repoLookup ::
  RepoName ->
  Map RepoMapKey (RepoConfig ConfigPhaseToml) ->
  Maybe (RepoConfig ConfigPhaseToml)
repoLookup repoName mp = lookupKey <|> lookupName
  where
    lookupKey = Map.lookup (RepoMapKeyCustom repoName.unRepoName) mp
    lookupName = Map.lookup (RepoMapKeyName repoName) mp
