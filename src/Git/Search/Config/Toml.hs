{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Git.Search.Config.Toml
  ( Toml (..),
  )
where

import Data.Map.Strict qualified as Map
import Git.Search.Config.Data
  ( Config (MkConfig, auth, clean, logColor, logLevel, repo),
    RepoConfig (MkRepoConfig, name),
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

      decodeMap :: Decoder (Map RepoName (RepoConfig ConfigPhaseToml))
      decodeMap = do
        assocVals <- fmap (\r -> (r.name, r)) <$> tomlDecoder
        pure $ Map.fromList assocVals

      decodeRepoDefaults :: Decoder (RepoConfig ConfigPhaseTomlRepoDefaults)
      decodeRepoDefaults = do
        fromMaybe (MkRepoConfig () () Nothing () () ())
          <$> getFieldOptWith tomlDecoder "repo-defaults"

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
