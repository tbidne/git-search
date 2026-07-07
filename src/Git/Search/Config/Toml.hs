module Git.Search.Config.Toml
  ( Toml (..),
  )
where

import Data.Map.Strict qualified as Map
import Git.Search.Config.Data
  ( Config (MkConfig, branches, clean, debug, repo),
    ConfigPhase (ConfigPhaseToml),
    RepoConfig (MkRepoConfig, domain, name, protocol),
  )
import Git.Search.Prelude
import TOML (Decoder, getFieldOptWith, getFieldWith)

newtype Toml = MkToml {coreConfig :: Config ConfigPhaseToml}

instance DecodeTOML Toml where
  tomlDecoder = do
    branches <- decodeBranches
    (domain, name, protocol) <- decodeRepo
    (clean, debug) <- decodeMisc

    pure
      MkToml
        { coreConfig =
            MkConfig
              { branches,
                clean,
                debug,
                repo =
                  MkRepoConfig
                    { domain,
                      name,
                      protocol
                    }
              }
        }
    where
      decodeBranches = getFieldOptWith decodeOsStrMap "branchMap"

      decodeRepo =
        fmap (fromMaybe (Nothing, Nothing, Nothing))
          $ flip getFieldOptWith "repository"
          $ do
            (,,)
              <$> getFieldOptWith decodeOsString "domain"
              <*> getFieldOptWith decodeOsString "name"
              <*> getFieldOptWith tomlDecoder "protocol"

      decodeMisc =
        fmap (fromMaybe (Nothing, Nothing))
          $ flip getFieldOptWith "miscellaneous"
          $ do
            (,)
              <$> getFieldOptWith decodeSwitch "clean"
              <*> getFieldOptWith decodeSwitch "debug"

decodeOsStrMap :: Decoder (Map OsString [OsString])
decodeOsStrMap = do
  assocStrs <- tomlDecoder
  assocOsStrs <- for assocStrs $ \(MkBranchItem (k, vs)) -> do
    kOsStr <- encodeFail k
    vsOsStr <- traverse encodeFail vs
    pure (kOsStr, vsOsStr)
  pure $ Map.fromList assocOsStrs

newtype BranchItem = MkBranchItem (String, [String])

instance DecodeTOML BranchItem where
  tomlDecoder = do
    name <- getFieldWith tomlDecoder "name"
    branches <- getFieldWith tomlDecoder "branches"
    pure $ MkBranchItem (name, branches)

decodeOsString :: Decoder OsString
decodeOsString = tomlDecoder >>= encodeFail

decodeSwitch :: Decoder Bool
decodeSwitch =
  tomlDecoder >>= \case
    "off" -> pure False
    "on" -> pure True
    other -> fail $ "Unrecognized: " ++ other
