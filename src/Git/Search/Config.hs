{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Git.Search.Config
  ( -- * Env
    Env (..),
    toEnv,

    -- * Misc
    getRepoPath,
  )
where

import Effectful.FileSystem.PathReader.Dynamic qualified as PR
import Effectful.FileSystem.PathWriter.Dynamic qualified as PW
import FileSystem.Path qualified as FS.Path
import Git.Search.Config.Args (Args (command))
import Git.Search.Config.Data
  ( Command (DeleteCache, SearchCommit, SearchPullRequest),
    Config (MkConfig, auth, clean, logColor, logLevel, repo),
    DeleteCacheType (DeleteCacheGlobal, DeleteCacheLocal),
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
    SearchCommitParams (MkSearchCommitParams),
    SearchPullRequestParams (MkSearchPullRequestParams),
  )
import Git.Search.Config.Merged (MergedConfig (coreConfig, key), mergeConfig)
import Git.Search.Config.Phase (ConfigPhase (ConfigPhaseArgs2, ConfigPhaseEnv))
import Git.Search.Config.Toml (Toml)
import Git.Search.Data
  ( Domain (unDomain),
    Protocol (ProtocolHttps, ProtocolSsh),
    RepoName (MkRepoName),
    RepoPath (MkRepoPath),
    RepoRemoteUri (MkRepoRemoteUri),
  )
import Git.Search.Prelude

newtype Env = MkEnv {coreConfig :: Config ConfigPhaseEnv}
  deriving stock (Eq, Show)

-- | Evolves the CLI Args to runtime Env.
toEnv ::
  ( HasCallStack,
    PathReader :> es,
    PathWriter :> es
  ) =>
  Args ConfigPhaseArgs2 ->
  Maybe Toml ->
  Eff es (Env, Command ConfigPhaseEnv)
toEnv args mToml = do
  let merged = mergeConfig args mToml

  command <- case args.command of
    DeleteCache () -> mkDeleteCache merged
    SearchCommit commit -> mkSearchCommit merged commit
    SearchPullRequest prNum -> mkSearchPullRequest merged prNum

  pure
    ( MkEnv
        { coreConfig =
            MkConfig
              { auth = merged.coreConfig.auth,
                clean = merged.coreConfig.clean,
                logColor = merged.coreConfig.logColor,
                logLevel = merged.coreConfig.logLevel,
                repo =
                  MkRepoConfig
                    { branches = merged.coreConfig.repo.branches,
                      domain = (),
                      key = (),
                      name = (),
                      path = (),
                      protocol = (),
                      remoteName = merged.coreConfig.repo.remoteName
                    }
              }
        },
      command
    )
  where
    mkDeleteCache merged =
      case merged.coreConfig.repo.name of
        Nothing ->
          DeleteCache . DeleteCacheGlobal <$> getCacheRoot
        Just repoName -> do
          -- NOTE: Ignoring merged.coreConfig.repo.path as it is irrelevant
          -- for deleting the cache.
          repoPath <- getRepoPath repoName merged.key Nothing
          pure $ DeleteCache (DeleteCacheLocal repoPath)

    mkSearchCommit merged commit = do
      repoName@(MkRepoName name) <- getName "search-commit" merged

      let domain = merged.coreConfig.repo.domain.unDomain
          protocol = merged.coreConfig.repo.protocol

          prefix = case protocol of
            ProtocolHttps -> mkHttps domain
            ProtocolSsh -> mkSsh domain

          src = prefix <> name

          repoSrc = MkRepoRemoteUri src

      repoPath <- getRepoPath repoName merged.key merged.coreConfig.repo.path

      pure $ SearchCommit $ MkSearchCommitParams (commit, repoPath, repoSrc)

    mkSearchPullRequest merged prNum = do
      repoName@(MkRepoName name) <- getName "search-pr" merged

      let domain = merged.coreConfig.repo.domain.unDomain
          protocol = merged.coreConfig.repo.protocol

      unless (domain == [osstr|github.com|]) $ do
        throwString
          $ "search-pr: --domain must be github.com, not: "
          ++ decodeLenient domain

      prefix <- case protocol of
        ProtocolHttps -> pure $ mkHttps domain
        ProtocolSsh ->
          throwString "search-pr: --protocol must be https, not: ssh"

      let src = prefix <> name

          repoSrc = MkRepoRemoteUri src

      repoPath <- getRepoPath repoName merged.key merged.coreConfig.repo.path

      pure $ SearchPullRequest $ MkSearchPullRequestParams (prNum, repoPath, repoSrc, repoName)

-- NOTE: [Repo Path Duplicates]
--
-- The user has the option to supply a file-system path to use for the repo
-- clone, either on the CLI or the toml. Otherwise we derive the path
--
--   <cache>/<name>
--
-- e.g.
--
-- ~/.cache/git-search/nixos/nixpkgs
--
-- In general, it is up to the user to anticipate collisions, and we do
-- not do any sort of validation on whether an extant repo corresponds
-- to the one we are trying to clone/fetch.
--
-- That said, because the repo-map supports identically named repos, we will
-- collide without explicit repo-map.paths, hence we can improve the situation.
--
-- First, we document that path exists, and explain why the user might want
-- to set it.
--
-- Second, for repo-map entries that have a 'key' (hence might collide),
-- we instead derive
--
--   <cache>/<key>
--
-- e.g.
--
-- ~/.cache/git-search/user-key

-- | Returns the file-system path to use for the repository clone.
-- If we are given an explicit path to use, use it. Otherwise, derived
-- path is <xdg_cache>/<repo_name>.
getRepoPath ::
  ( HasCallStack,
    PathReader :> es,
    PathWriter :> es
  ) =>
  -- | Repo name.
  RepoName ->
  -- | Toml repo-map key field.
  Maybe OsString ->
  -- | Config repo path, on the file-system.
  Maybe OsPath ->
  Eff es RepoPath
getRepoPath (MkRepoName name) mKey Nothing = do
  -- No config repo path: derived path is cache/<name>.
  root <- getCacheRoot
  -- If the toml repo-map.key exists, use it instead, as it must be unique.
  -- We previously combined them i.e. 'maybe name (</> name) mKey',
  -- but this is unnecessary verbosity, as keys are unique.
  pathRel <- FS.Path.parseRelDir $ fromMaybe name mKey
  pure $ MkRepoPath $ root <</>> pathRel
getRepoPath _ _ (Just rawPath) = do
  -- Repo path p: Use it.
  path <- PR.expandTilde rawPath
  MkRepoPath <$> FS.Path.parseAbsDir path

getCacheRoot ::
  ( HasCallStack,
    PathReader :> es,
    PathWriter :> es
  ) =>
  Eff es (Path Abs Dir)
getCacheRoot = do
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

  pure root

getCacheDir :: (HasCallStack, PathReader :> es) => Eff es (Path Abs Dir)
getCacheDir =
  PR.getXdgCache [osstr|git-search|] >>= FS.Path.parseAbsDir

getName :: (HasCallStack) => String -> MergedConfig -> Eff es RepoName
getName cmdName merged = case merged.coreConfig.repo.name of
  Just n -> pure n
  Nothing ->
    throwString
      $ mconcat
        [ cmdName,
          ": Repository name must be specified by CLI ",
          "args or Toml config."
        ]

-- OsString not OsPath since we want slashes preserved.
mkHttps :: OsString -> OsString
mkHttps domain = [osstr|https://|] <> domain <> [osstr|/|]

mkSsh :: OsString -> OsString
mkSsh domain = [osstr|git@|] <> domain <> [osstr|:|]

makeFieldLabelsNoPrefix ''Env
