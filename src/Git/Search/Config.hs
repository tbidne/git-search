{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}

module Git.Search.Config
  ( Env (..),
    toEnv,
  )
where

import Effectful.FileSystem.PathReader.Static qualified as PR
import Effectful.FileSystem.PathWriter.Static qualified as PW
import FileSystem.Path qualified as FS.Path
import Git.Search.Config.Args (Args)
import Git.Search.Config.Data
  ( Config (MkConfig, branches, clean, debug, repo),
    ConfigPhase (ConfigPhaseEnv),
    Protocol (ProtocolHttps, ProtocolSsh),
    RepoConfig (domain, name, protocol),
    RepoEnv (MkRepoEnv, path, src),
  )
import Git.Search.Config.Merged (MergedConfig (coreConfig), mergeConfig)
import Git.Search.Config.Toml (Toml)
import Git.Search.Prelude

newtype Env = MkEnv {coreConfig :: Config ConfigPhaseEnv}

-- | Evolves the CLI Args to runtime Env.
toEnv ::
  ( HasCallStack,
    PathReader :> es,
    PathWriter :> es
  ) =>
  Args ->
  Maybe Toml ->
  Eff es Env
toEnv args mToml = do
  merged <- mergeConfig args mToml

  let -- OsString not OsPath since we want slashes preserved.
      prefix = case merged.coreConfig.repo.protocol of
        ProtocolHttps -> [osstr|https://|] <> merged.coreConfig.repo.domain <> [osstr|/|]
        ProtocolSsh -> [osstr|git@|] <> merged.coreConfig.repo.domain <> [osstr|:|]

      src = prefix <> merged.coreConfig.repo.name

  -- We get the rootOsP in two steps, rather than the direct
  --
  --   root@(MkPath rootOsP) <- ...
  --
  -- because GHC 9.10 + Effectful incorrectly thinks this is a
  -- failable pattern, hence requires Fail :> es.
  root <- getCacheDir
  let MkPath rootOsP = root
  -- E.g. root := ~/.cache/git-search
  --
  -- Hence, repo := ~/.cache/git-search/org/some-repo
  --
  -- Create cache if it does not exist. The clone step will
  -- take care of creating the repo directory if necessary.
  PW.createDirectoryIfMissing True rootOsP

  pathRel <- FS.Path.parseRelDir merged.coreConfig.repo.name
  let path = root <</>> pathRel
      repo =
        MkRepoEnv
          { path,
            src
          }

  pure
    $ MkEnv
      { coreConfig =
          MkConfig
            { branches = merged.coreConfig.branches,
              clean = merged.coreConfig.clean,
              debug = merged.coreConfig.debug,
              repo
            }
      }

getCacheDir :: (HasCallStack, PathReader :> es) => Eff es (Path Abs Dir)
getCacheDir =
  PR.getXdgCache [osstr|git-search|] >>= FS.Path.parseAbsDir
