{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Git.Search.Config.Toml
  ( Toml (..),
  )
where

import Data.List qualified as L
import Data.Map.Strict qualified as Map
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
      ( ConfigPhaseToml,
        ConfigPhaseTomlRepoDefaults
      ),
  )
import Git.Search.Data (Commit, RepoName)
import Git.Search.Prelude
import TOML (Decoder, getArrayOf)

data Toml = MkToml
  { coreConfig :: Config ConfigPhaseToml,
    repoDefaults :: RepoConfig ConfigPhaseTomlRepoDefaults,
    searchDefaults :: [(Either Commit Word32, RepoName, Maybe Text)]
  }

instance DecodeTOML Toml where
  tomlDecoder = do
    (logColor, logLevel) <- decodeLogging
    repo <- decodeRepoMap
    clean <- decodeMisc
    repoDefaults <- decodeRepoDefaults
    searchDefaults <- decodeSearchDefaults

    pure
      MkToml
        { coreConfig =
            MkConfig
              { auth = (),
                clean,
                logColor,
                logLevel,
                repo
              },
          repoDefaults,
          searchDefaults
        }
    where
      decodeLogging =
        fmap (fromMaybe (Nothing, Nothing))
          $ flip getFieldOptWith "logging"
          $ do
            (,)
              <$> getFieldOptWith decodeSwitch "log-color"
              <*> getFieldOptWith tomlDecoder "log-level"

      decodeRepoMap = do
        getFieldOptWith decodeMap "repo-map" <&> \case
          Nothing -> Map.empty
          Just mp -> mp

      decodeMap :: Decoder (Map RepoMapKey (RepoConfig ConfigPhaseToml))
      decodeMap = do
        (keyVals, nameVals) <- L.foldl' @[] go ([], []) <$> tomlDecoder
        pure $ Map.fromList (keyVals ++ nameVals)
        where
          go (keys, names) r = case r.key of
            -- 1. Key does not exist, so add the name.
            Nothing -> (keys, (RepoMapKeyName r.name, r) : names)
            -- 2. Key does exist, so add it, skip the name.
            Just k -> ((RepoMapKeyCustom k, r) : keys, names)

      decodeRepoDefaults :: Decoder (RepoConfig ConfigPhaseTomlRepoDefaults)
      decodeRepoDefaults = do
        fromMaybe defRepo
          <$> getFieldOptWith tomlDecoder "repo-defaults"
        where
          defRepo =
            MkRepoConfig
              { branches = (),
                domain = (),
                key = (),
                name = Nothing,
                path = (),
                protocol = (),
                remoteName = ()
              }

      decodeSearchDefaults :: Decoder [(Either Commit Word32, RepoName, Maybe Text)]
      decodeSearchDefaults =
        fmap (fromMaybe [])
          . flip getFieldOptWith "search-defaults"
          $ getArrayOf decodeSearchDefault

      decodeSearchDefault :: Decoder (Either Commit Word32, RepoName, Maybe Text)
      decodeSearchDefault = do
        cmdArg <-
          getFieldWith @Text tomlDecoder "cmd" >>= \case
            "commit" -> Left <$> decodeSearchDefaultArg
            "pr" -> Right <$> decodeSearchDefaultArg
            other -> fail $ "Unrecognized command: " ++ unpack other

        -- NOTE: [SearchDefaults name]
        --
        -- We require the name to be set. Technically, this need not be the
        -- case, as we could imagine:
        --
        --   1. Passing in the --name on the CLI.
        --   2. Using repo-defaults.name from the TOML.
        --
        -- But these make the semantics confusing for arguably very little
        -- (any?) gain. It is simpler to require each search to specify its
        -- repo.
        repoName <- getFieldWith tomlDecoder "name"
        mDesc <- getFieldOptWith tomlDecoder "desc"

        pure (cmdArg, repoName, mDesc)

      decodeSearchDefaultArg :: (DecodeTOML a) => Decoder a
      decodeSearchDefaultArg = getFieldWith tomlDecoder "arg"

      decodeMisc =
        fmap (fromMaybe Nothing)
          $ flip getFieldOptWith "miscellaneous"
          $ getFieldOptWith decodeSwitch "clean"

decodeSwitch :: Decoder Bool
decodeSwitch =
  tomlDecoder >>= \case
    "off" -> pure False
    "on" -> pure True
    other -> fail $ "Unrecognized: " ++ other

makeFieldLabelsNoPrefix ''Toml
