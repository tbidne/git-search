{-# LANGUAGE QuasiQuotes #-}

module Git.Search
  ( -- * Searching
    searchCommit,
    searchPullRequest,

    -- * Deleting
    deleteCache,
  )
where

import Control.Exception.Utils qualified as Ex.Utils
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Data.Time.Relative (Format (verbosity))
import Data.Time.Relative qualified as Time.Rel
import Effectful.FileSystem.PathReader.Static qualified as PR
import Effectful.FileSystem.PathWriter.Static qualified as PW
import Effectful.Process qualified as P
import Effectful.Time.Static qualified as Time
import Git.Search.Config
  ( Env (branches, coreConfig),
  )
import Git.Search.Config.Data
  ( Config (clean),
    DeleteCacheType (DeleteCacheGlobal, DeleteCacheLocal),
  )
import Git.Search.Data
  ( Commit (MkCommit, unCommit),
    RepoName (unRepoName),
    RepoPath (unRepoPath),
    RepoRemoteUri (unRepoRemoteUri),
  )
import Git.Search.Logging qualified as Logging
import Git.Search.Network (Network)
import Git.Search.Network qualified as Network
import Git.Search.Prelude

-- | Deletes the cache.
deleteCache ::
  ( HasCallStack,
    PathReader :> es,
    PathWriter :> es,
    Reader Env :> es,
    Terminal :> es
  ) =>
  DeleteCacheType ->
  Eff es ()
deleteCache cacheType = do
  case cacheType of
    DeleteCacheGlobal (MkPath cacheDir) -> do
      Logging.logDebug
        $ "Deleting cache: "
        <> decodeLenient cacheDir

      PW.removePathForcibly cacheDir
    DeleteCacheLocal repoPath -> do
      let repoDir = repoPathToOsP repoPath
          repoDirStr = decodeLenient repoDir

      Logging.logDebug
        $ "Deleting cache repo: "
        <> repoDirStr

      exists <- PR.doesDirectoryExist repoDir
      if exists
        then PW.removeDirectory repoDir
        else throwString $ "Cached repository does not exist: " <> repoDirStr

-- | Returns a list of branches matching the search criteria.
searchCommit ::
  ( HasCallStack,
    PathReader :> es,
    PathWriter :> es,
    Process :> es,
    Reader Env :> es,
    Terminal :> es,
    Time :> es
  ) =>
  (Commit, RepoPath, RepoRemoteUri) ->
  Eff es [Text]
searchCommit (commit, repoPath, repoSrc) = do
  cloneRepo repoPath repoSrc
  findBranches commit repoPath

searchPullRequest ::
  ( HasCallStack,
    PathReader :> es,
    PathWriter :> es,
    Process :> es,
    Network :> es,
    Reader Env :> es,
    Terminal :> es,
    Time :> es
  ) =>
  (Word32, RepoPath, RepoRemoteUri, RepoName) ->
  Eff es [Text]
searchPullRequest (prNum, repoPath, repoSrc, repoName) = do
  commit <- prToCommit prNum repoName
  searchCommit (commit, repoPath, repoSrc)

prToCommit ::
  ( HasCallStack,
    Network :> es,
    Reader Env :> es,
    Terminal :> es
  ) =>
  Word32 ->
  RepoName ->
  Eff es Commit
prToCommit prNum repoName = do
  manager <- Network.newTlsManager

  baseReq <- Network.mkJsonRequest baseUrl
  pr <- Network.runJsonRequest @GitPullRequest commitsUrl manager baseReq

  when (pr.state == "open")
    $ Logging.logInfo
    $ "Pull request "
    ++ prStr
    ++ " is currently open."

  commitsReq <- Network.mkJsonRequest commitsUrl
  commits <- Network.runJsonRequest @[GitCommit] commitsUrl manager commitsReq

  case commits of
    [] -> throwString "No commits found."
    (c : cs) -> do
      let latest = NE.last (c :| cs)
          latestStr = unpack latest.sha

      Logging.logDebug $ "Found commit: " ++ latestStr

      MkCommit <$> encodeThrowM latestStr
  where
    prStr = show prNum

    baseUrl =
      mconcat
        [ "https://api.github.com/repos/",
          decodeLenient repoName.unRepoName,
          "/pulls/",
          prStr
        ]

    commitsUrl = baseUrl <> "/commits"

newtype GitPullRequest = MkGitPullRequest
  { state :: Text
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

newtype GitCommit = MkGitCommit
  { sha :: Text
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

cloneRepo ::
  ( HasCallStack,
    PathReader :> es,
    PathWriter :> es,
    Process :> es,
    Reader Env :> es,
    Terminal :> es,
    Time :> es
  ) =>
  RepoPath ->
  RepoRemoteUri ->
  Eff es ()
cloneRepo repoPath repoSrc = do
  env <- ask @Env

  Logging.logDebug $ "Clone destination: " <> repoPathStr

  exists <- PR.doesDirectoryExist repoPathOsP

  if exists
    then do
      if env.coreConfig.clean
        then do
          -- 1. Repo exists but clean is active: delete and clone.
          PW.removeDirectoryRecursive repoPathOsP
          runClone
        else
          -- 2. Repo exists but clean is not active: update.
          runFetch
    else
      -- 3. Repo does not exist: clone.
      runClone
  where
    runClone = do
      Logging.logInfo $ "Cloning " ++ repoSrcStr ++ "..."
      timeStr <- withTiming_ $ runGit_ cloneArgs
      Logging.logInfo $ "Clone finished: " ++ timeStr

    runFetch = do
      Logging.logInfo $ "Fetching " ++ repoSrcStr ++ "..."
      timeStr <- PW.withCurrentDirectory (repoPathToOsP repoPath) $ do
        withTiming_ $ runGit_ fetchArgs
      Logging.logInfo $ "Fetch finished: " ++ timeStr

    -- Our args will make a bare repo with no files e.g. git clone org/some-repo
    -- in ~/.cache/git-search will create
    --
    --   ~/.cache/git-search/.git
    --
    -- But we still want our directory to be namespaced by repo name, hence
    -- we clone to repoPathOsP i.e. ~/.cache/git-search/org/some-repo.
    cloneArgs =
      [ [osstr|clone|],
        [osstr|--no-checkout|],
        [osstr|--filter=blob:none|],
        [osstr|--|],
        repoSrc.unRepoRemoteUri,
        repoPathOsP
      ]

    fetchArgs =
      [ [osstr|fetch|],
        [osstr|--prune|]
      ]

    repoPathOsP = toOsPath repoPath.unRepoPath
    repoPathStr = decodeLenient repoPathOsP
    repoSrcStr = decodeLenient repoSrc.unRepoRemoteUri

findBranches ::
  ( HasCallStack,
    PathWriter :> es,
    Process :> es,
    Reader Env :> es,
    Terminal :> es,
    Time :> es
  ) =>
  Commit ->
  RepoPath ->
  Eff es [Text]
findBranches commit repoPath = do
  env <- ask @Env

  Logging.logInfo
    $ "Searching for hash "
    ++ hashStr
    ++ "..."

  commitExists <- doesCommitExist commit repoPath

  if not commitExists
    then do
      Logging.logInfo
        "Commit does not exist."
      pure []
    else do
      PW.withCurrentDirectory (repoPathToOsP repoPath) $ do
        (timeStr, out) <- withTiming $ runGitOut (gitArgs env.branches)
        Logging.logInfo
          $ "Search finished: "
          ++ timeStr
        toText <$> decodeThrowM out
  where
    gitArgs branches = case branches of
      [] -> gitDefArgs
      bs@(_ : _) -> gitBranchArgs bs

    gitDefArgs =
      [ [osstr|branch|],
        [osstr|-r|],
        [osstr|--contains|],
        commit.unCommit
      ]

    gitBranchArgs bs =
      [ [osstr|branch|],
        [osstr|-r|],
        [osstr|--contains|],
        commit.unCommit,
        [osstr|--list|]
      ]
        ++ bs

    toText = fmap T.strip . T.lines . pack

    hashStr = decodeLenient commit.unCommit

doesCommitExist ::
  ( HasCallStack,
    PathWriter :> es,
    Process :> es,
    Reader Env :> es,
    Terminal :> es
  ) =>
  Commit ->
  RepoPath ->
  Eff es Bool
doesCommitExist commit repoPath = do
  (ec, _, _) <-
    PW.withCurrentDirectory (repoPathToOsP repoPath) $ runGit gitArgs
  pure $ case ec of
    ExitSuccess -> True
    ExitFailure _ -> False
  where
    gitArgs =
      [ [osstr|cat-file|],
        [osstr|-e|],
        commit.unCommit
      ]

runGitOut ::
  ( HasCallStack,
    Process :> es,
    Reader Env :> es,
    Terminal :> es
  ) =>
  [OsString] ->
  Eff es OsString
runGitOut args = runProcOut [osstr|git|] args

runGit ::
  ( HasCallStack,
    Process :> es,
    Reader Env :> es,
    Terminal :> es
  ) =>
  [OsString] ->
  Eff es (ExitCode, String, String)
runGit args = runProc [osstr|git|] args

runGit_ ::
  ( HasCallStack,
    Process :> es,
    Reader Env :> es,
    Terminal :> es
  ) =>
  [OsString] ->
  Eff es ()
runGit_ args = do
  (ec, _, err) <- runGit args
  case ec of
    ExitFailure _ -> do
      let argsStr = L.unwords $ fmap decodeLenient args
      Ex.Utils.throwString
        $ unpack
        $ mconcat
          [ "Error running git with args '",
            pack argsStr,
            "': ",
            pack err
          ]
    ExitSuccess -> pure ()

runProcOut ::
  ( HasCallStack,
    Process :> es,
    Reader Env :> es,
    Terminal :> es
  ) =>
  OsString ->
  [OsString] ->
  Eff es OsString
runProcOut exe args = do
  let exeStr = decodeLenient exe
  (ec, out, err) <- runProc exe args
  case ec of
    ExitFailure _ -> do
      let argsStr = L.unwords $ fmap decodeLenient args
      Ex.Utils.throwString
        $ unpack
        $ mconcat
          [ "Error running process '",
            pack exeStr,
            "' with args '",
            pack argsStr,
            "': ",
            pack err
          ]
    ExitSuccess -> encodeThrowM out

runProc ::
  ( HasCallStack,
    Process :> es,
    Reader Env :> es,
    Terminal :> es
  ) =>
  OsString ->
  [OsString] ->
  Eff es (ExitCode, String, String)
runProc exe args = do
  exeStr <- decodeThrowM exe
  argsStrs <- traverse decodeThrowM args

  let msg =
        mconcat
          [ "runProcess: ",
            L.unwords (exeStr : argsStrs)
          ]

  Logging.logDebug msg

  P.readProcessWithExitCode exeStr argsStrs name
  where
    name = "runProcess"

withTiming ::
  ( HasCallStack,
    Time :> es
  ) =>
  Eff es a ->
  Eff es (String, a)
withTiming m = do
  (ts, r) <- Time.withTiming m
  let tsStr =
        Time.Rel.formatRelativeTime fmt
          . Time.Rel.fromSeconds
          . floor
          . Time.toSeconds
          $ ts
  pure (tsStr, r)
  where
    fmt =
      Time.Rel.MkFormat
        { style = Time.Rel.FormatStyleProse,
          verbosity = Time.Rel.FormatVerbosityCompact
        }

withTiming_ :: (HasCallStack, Time :> es) => Eff es a -> Eff es String
withTiming_ = fmap fst . withTiming

repoPathToOsP :: RepoPath -> OsPath
repoPathToOsP = toOsPath . (.unRepoPath)
