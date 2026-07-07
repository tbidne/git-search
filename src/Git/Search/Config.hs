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
import Git.Search.Config.Args (Args (command))
import Git.Search.Config.Data
  ( Command (DeleteCache, SearchCommit, SearchPullRequest),
    Config (MkConfig, branches, clean, logColor, logLevel, repo),
    ConfigPhase (ConfigPhaseEnv),
    DeleteCacheType (DeleteCacheGlobal, DeleteCacheLocal),
    Protocol (ProtocolHttps, ProtocolSsh),
    RepoConfig (domain, name, protocol),
    RepoName (MkRepoName),
    RepoPath (MkRepoPath),
    RepoSrc (MkRepoSrc),
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
  Eff es (Env, Command ConfigPhaseEnv)
toEnv args mToml = do
  let merged = mergeConfig args mToml

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

  command <- case args.command of
    DeleteCache () -> do
      case merged.coreConfig.repo.name of
        Nothing -> pure $ DeleteCache (DeleteCacheGlobal root)
        Just (MkRepoName name) -> do
          pathRel <- FS.Path.parseRelDir name
          let path = root <</>> pathRel
              repoPath = MkRepoPath path
          pure $ DeleteCache (DeleteCacheLocal repoPath)
    SearchCommit commit -> do
      MkRepoName name <- case merged.coreConfig.repo.name of
        Just n -> pure n
        Nothing ->
          throwString
            $ mconcat
              [ "search-commit: Repository name must be specified by CLI ",
                "args or Toml config."
              ]

      let -- OsString not OsPath since we want slashes preserved.
          prefix = case merged.coreConfig.repo.protocol of
            ProtocolHttps -> [osstr|https://|] <> merged.coreConfig.repo.domain <> [osstr|/|]
            ProtocolSsh -> [osstr|git@|] <> merged.coreConfig.repo.domain <> [osstr|:|]

          src = prefix <> name

      pathRel <- FS.Path.parseRelDir name
      let path = root <</>> pathRel
          repoPath = MkRepoPath path
          repoSrc = MkRepoSrc src

      pure $ SearchCommit (commit, repoPath, repoSrc)
    SearchPullRequest prNum -> do
      repoName@(MkRepoName name) <- case merged.coreConfig.repo.name of
        Just n -> pure n
        Nothing ->
          throwString
            $ mconcat
              [ "search-pr: Repository name must be specified by CLI ",
                "args or Toml config."
              ]

      let domain = merged.coreConfig.repo.domain
          protocol = merged.coreConfig.repo.protocol

      unless (domain == [osstr|github.com|]) $ do
        throwString
          $ "search-pr: --domain must be github.com, not: "
          ++ decodeLenient domain

      case protocol of
        ProtocolHttps -> pure ()
        ProtocolSsh ->
          throwString "search-pr: --protocol must be https, not: ssh"

      let -- OsString not OsPath since we want slashes preserved.
          prefix = case merged.coreConfig.repo.protocol of
            ProtocolHttps -> [osstr|https://|] <> merged.coreConfig.repo.domain <> [osstr|/|]
            ProtocolSsh -> [osstr|git@|] <> merged.coreConfig.repo.domain <> [osstr|:|]

          src = prefix <> name

      pathRel <- FS.Path.parseRelDir name
      let path = root <</>> pathRel
          repoPath = MkRepoPath path
          repoSrc = MkRepoSrc src

      pure $ SearchPullRequest (prNum, repoPath, repoSrc, repoName)

  pure
    ( MkEnv
        { coreConfig =
            MkConfig
              { branches = merged.coreConfig.branches,
                clean = merged.coreConfig.clean,
                logColor = merged.coreConfig.logColor,
                logLevel = merged.coreConfig.logLevel,
                repo = ()
              }
        },
      command
    )

getCacheDir :: (HasCallStack, PathReader :> es) => Eff es (Path Abs Dir)
getCacheDir =
  PR.getXdgCache [osstr|git-search|] >>= FS.Path.parseAbsDir
