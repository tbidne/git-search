module Git.Search.Config.Toml
  ( Toml (..),
  )
where

import Data.Map.Strict qualified as Map
import Git.Search.Config.Data
  ( Config (MkConfig, clean, logColor, logLevel, repo),
    RepoMapVal,
  )
import Git.Search.Config.Phase (ConfigPhase (ConfigPhaseToml))
import Git.Search.Data (RepoName (MkRepoName))
import Git.Search.Prelude
import TOML (Decoder)

newtype Toml = MkToml {coreConfig :: Config ConfigPhaseToml}

instance DecodeTOML Toml where
  tomlDecoder = do
    (logColor, logLevel) <- decodeLogging
    repo <- decodeRepoMap
    clean <- decodeMisc

    pure
      MkToml
        { coreConfig =
            MkConfig
              { clean,
                logColor,
                logLevel,
                repo
              }
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

      decodeMap :: Decoder (Map RepoName RepoMapVal)
      decodeMap = do
        assocVals <- fmap (.unRepoMapItem) <$> tomlDecoder @[RepoMapItem]
        pure $ Map.fromList assocVals

      decodeMisc =
        fmap (fromMaybe Nothing)
          $ flip getFieldOptWith "miscellaneous"
          $ getFieldOptWith decodeSwitch "clean"

newtype RepoMapItem = MkRepoMapItem {unRepoMapItem :: (RepoName, RepoMapVal)}

instance DecodeTOML RepoMapItem where
  tomlDecoder = do
    name <- encodeFail =<< getFieldWith tomlDecoder "name"
    rmv <- tomlDecoder
    pure $ MkRepoMapItem (MkRepoName name, rmv)

decodeSwitch :: Decoder Bool
decodeSwitch =
  tomlDecoder >>= \case
    "off" -> pure False
    "on" -> pure True
    other -> fail $ "Unrecognized: " ++ other
