{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Git.Search.Config.Merged
  ( MergedConfig (..),
    mergeConfig,
  )
where

import Data.Map.Strict qualified as Map
import Git.Search.Config.Args (Args)
import Git.Search.Config.Data
  ( Config (MkConfig, auth, logColor, logLevel, repo, update),
    RepoConfig
      ( MkRepoConfig,
        branches,
        branchesPrefix,
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
import Git.Search.Config.Toml (Toml)
import Git.Search.Config.WithDisabled (WithDisabled (Disabled, With))
import Git.Search.Config.WithDisabled qualified as WD
import Git.Search.Data
  ( Domain (MkDomain),
    Protocol (ProtocolHttps),
    RepoName,
    RepoUpdate (RepoUpdateAuto),
  )
import Git.Search.Prelude

data MergedConfig = MkMergedConfig
  { -- | Core config.
    coreConfig :: Config ConfigPhaseMerged,
    -- | The 'key' field, if we matched toml repo-map config that has a 'key'.
    -- This is used for the file-system path.
    key :: Maybe OsString
  }

makeFieldLabelsNoPrefix ''MergedConfig

mergeConfig ::
  Args ConfigPhaseArgs2 ->
  Maybe Toml ->
  MergedConfig
mergeConfig args mToml =
  MkMergedConfig
    { coreConfig,
      key
    }
  where
    -- initName is either the CLI name or toml default name.
    initName =
      mergeMaybe
        (args ^. #coreConfig % #repo % #name)
        (mToml ^? _Just % #repoDefaults % #name % _Just)

    (name, mRepo, key) = case initName of
      Nothing -> (Nothing, Nothing, Nothing)
      Just n -> case (mToml ^? _Just % #coreConfig % #repo) >>= repoLookup n of
        Nothing -> (Just n, Nothing, Nothing)
        Just r -> (Just $ r ^. #name, Just r, r ^. #key)

    coreConfig = case mToml of
      Nothing ->
        MkConfig
          { auth = args ^. #coreConfig % #auth,
            logColor = mergeBoolTrue (args ^. #coreConfig % #logColor) Nothing,
            logLevel =
              mergeMaybe
                (args ^. #coreConfig % #logLevel)
                Nothing,
            repo =
              MkRepoConfig
                { branches =
                    mergeBranches
                      (args ^. #coreConfig % #repo % #branches)
                      Nothing,
                  branchesPrefix =
                    mergeBoolTrue
                      (args ^. #coreConfig % #repo % #branchesPrefix)
                      Nothing,
                  domain =
                    mergeWD
                      defDomain
                      (view #domain)
                      (args ^. #coreConfig % #repo % #domain)
                      Nothing,
                  key = (),
                  name,
                  path =
                    mergeWDMaybe
                      (view #path)
                      (args ^. #coreConfig % #repo % #path)
                      Nothing,
                  protocol =
                    mergeWD
                      defProtocol
                      (view #protocol)
                      (args ^. #coreConfig % #repo % #protocol)
                      Nothing,
                  remoteName =
                    mergeWDMaybe
                      (view #remoteName)
                      (args ^. #coreConfig % #repo % #remoteName)
                      Nothing
                },
            update = mergeUpdate (args ^. #coreConfig % #update) Nothing
          }
      Just toml ->
        MkConfig
          { auth = args ^. #coreConfig % #auth,
            logColor =
              mergeBoolTrue
                (args ^. #coreConfig % #logColor)
                (toml ^. #coreConfig % #logColor),
            logLevel =
              mergeMaybe
                (args ^. #coreConfig % #logLevel)
                (toml ^. #coreConfig % #logLevel),
            repo =
              MkRepoConfig
                { branches =
                    mergeBranches
                      (args ^. #coreConfig % #repo % #branches)
                      mRepo,
                  branchesPrefix =
                    mergeBoolTrue
                      (args ^. #coreConfig % #repo % #branchesPrefix)
                      (mRepo ^? _Just % #branchesPrefix % _Just),
                  domain =
                    mergeWD
                      defDomain
                      (view #domain)
                      (args ^. #coreConfig % #repo % #domain)
                      mRepo,
                  key = (),
                  name,
                  path =
                    mergeWDMaybe
                      (view #path)
                      (args ^. #coreConfig % #repo % #path)
                      mRepo,
                  protocol =
                    mergeWD
                      defProtocol
                      (view #protocol)
                      (args ^. #coreConfig % #repo % #protocol)
                      mRepo,
                  remoteName =
                    mergeWDMaybe
                      (view #remoteName)
                      (args ^. #coreConfig % #repo % #remoteName)
                      mRepo
                },
            update =
              mergeUpdate
                (args ^. #coreConfig % #update)
                (toml ^. #coreConfig % #update)
          }

mergeBoolTrue :: Maybe Bool -> Maybe Bool -> Bool
mergeBoolTrue (Just b) _ = b
mergeBoolTrue Nothing (Just b) = b
mergeBoolTrue Nothing Nothing = True

mergeMaybe :: Maybe (WithDisabled a) -> Maybe a -> Maybe a
mergeMaybe (Just Disabled) _ = Nothing
mergeMaybe a b = WD.toMaybe a <|> b

mergeBranches ::
  Maybe (WithDisabled (List OsString)) ->
  Maybe (RepoConfig ConfigPhaseToml) ->
  List OsString
mergeBranches (Just Disabled) _ = []
mergeBranches (Just (With branches)) _ = branches
mergeBranches Nothing mRepo = maybe [] (view #branches) mRepo

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

mergeUpdate ::
  Maybe RepoUpdate ->
  Maybe RepoUpdate ->
  RepoUpdate
mergeUpdate (Just u1) _ = u1
mergeUpdate Nothing (Just u2) = u2
mergeUpdate Nothing Nothing = RepoUpdateAuto

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
    lookupKey = Map.lookup (RepoMapKeyCustom (repoName ^. #unRepoName)) mp
    lookupName = Map.lookup (RepoMapKeyName repoName) mp
