{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Git.Search.Config.Toml
  ( Toml (..),
  )
where

import Data.Map.Strict qualified as Map
import Git.Search.Config.Data
  ( Config (MkConfig, auth, clean, logColor, logLevel, repo),
    RepoConfig (name),
  )
import Git.Search.Config.Phase (ConfigPhase (ConfigPhaseToml))
import Git.Search.Data (Commit, RepoName)
import Git.Search.Prelude
import TOML (Decoder, getArrayOf)

data Toml = MkToml
  { coreConfig :: Config ConfigPhaseToml,
    searchDefaults :: [(Either Commit Word32, Maybe RepoName, Maybe Text)]
  }

instance DecodeTOML Toml where
  tomlDecoder = do
    (logColor, logLevel) <- decodeLogging
    repo <- decodeRepoMap
    clean <- decodeMisc
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

      decodeSearchDefaults :: Decoder [(Either Commit Word32, Maybe RepoName, Maybe Text)]
      decodeSearchDefaults =
        fmap (fromMaybe [])
          . flip getFieldOptWith "search-defaults"
          $ getArrayOf decodeSearchDefault

      decodeSearchDefault :: Decoder (Either Commit Word32, Maybe RepoName, Maybe Text)
      decodeSearchDefault = do
        cmdArg <-
          getFieldWith @Text tomlDecoder "cmd" >>= \case
            "commit" -> Left <$> decodeSearchDefaultArg
            "pr" -> Right <$> decodeSearchDefaultArg
            other -> fail $ "Unrecognized command: " ++ unpack other

        mRepoName <- getFieldOptWith tomlDecoder "name"
        mDesc <- getFieldOptWith tomlDecoder "desc"

        pure (cmdArg, mRepoName, mDesc)

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
