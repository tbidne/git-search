{-# LANGUAGE QuasiQuotes #-}

module Git.Search
  ( -- * Searching
    searchCommit,

    -- * Deleting
    deleteCache,
  )
where

import Control.Exception.Utils qualified as Ex.Utils
import Data.List qualified as L
import Data.Text qualified as T
import Data.Time.Relative (Format (verbosity))
import Data.Time.Relative qualified as Time.Rel
import Effectful.FileSystem.PathReader.Static qualified as PR
import Effectful.FileSystem.PathWriter.Static qualified as PW
import Effectful.Process qualified as P
import Effectful.Time.Static qualified as Time
import Git.Search.Config
  ( Env (coreConfig),
  )
import Git.Search.Config.Data
  ( Commit (unCommit),
    Config (branches, clean, debug),
    DeleteCacheType (DeleteCacheGlobal, DeleteCacheLocal),
    RepoPath (unRepoPath),
    RepoSrc (unRepoSrc),
  )
import Git.Search.Prelude

deleteCache ::
  ( HasCallStack,
    PathReader :> es,
    PathWriter :> es,
    Terminal :> es
  ) =>
  Env ->
  DeleteCacheType ->
  Eff es ()
deleteCache env cacheType = case cacheType of
  DeleteCacheGlobal (MkPath cacheDir) -> do
    when env.coreConfig.debug $ do
      putStrLn $ "Deleting cache: " <> decodeLenient cacheDir

    PW.removeDirectoryIfExists_ cacheDir
  DeleteCacheLocal repoPath -> do
    let repoDir = repoPathToOsP repoPath
        repoDirStr = decodeLenient repoDir

    when env.coreConfig.debug $ do
      putStrLn $ "Deleting cache repo: " <> repoDirStr

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
    Terminal :> es,
    Time :> es
  ) =>
  Env ->
  (Commit, RepoPath, RepoSrc) ->
  Eff es [Text]
searchCommit env (commit, repoPath, repoSrc) = do
  cloneRepo env repoPath repoSrc
  findBranches env commit repoPath

cloneRepo ::
  ( HasCallStack,
    PathReader :> es,
    PathWriter :> es,
    Process :> es,
    Terminal :> es,
    Time :> es
  ) =>
  Env ->
  RepoPath ->
  RepoSrc ->
  Eff es ()
cloneRepo env repoPath repoSrc = do
  when env.coreConfig.debug $ do
    putStrLn $ "Clone destination: " <> repoPathStr

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
      putStrLn $ "Cloning " ++ repoSrcStr ++ "..."
      timeStr <- withTiming_ $ runGit_ env cloneArgs
      putStrLn $ "Clone finished: " ++ timeStr

    runFetch = do
      putStrLn $ "Fetching " ++ repoSrcStr ++ "..."
      timeStr <- PW.withCurrentDirectory (repoPathToOsP repoPath) $ do
        withTiming_ $ runGit_ env fetchArgs
      putStrLn $ "Fetch finished: " ++ timeStr

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
        repoSrc.unRepoSrc,
        repoPathOsP
      ]

    fetchArgs =
      [ [osstr|fetch|],
        [osstr|--prune|]
      ]

    repoPathOsP = toOsPath repoPath.unRepoPath
    repoPathStr = decodeLenient repoPathOsP
    repoSrcStr = decodeLenient repoSrc.unRepoSrc

findBranches ::
  ( HasCallStack,
    PathWriter :> es,
    Process :> es,
    Terminal :> es,
    Time :> es
  ) =>
  Env ->
  Commit ->
  RepoPath ->
  Eff es [Text]
findBranches env commit repoPath = do
  putStrLn $ "Searching for hash " ++ hashStr ++ "..."

  commitExists <- doesCommitExist env commit repoPath

  if not commitExists
    then do
      putStrLn "Commit does not exist."
      pure []
    else do
      PW.withCurrentDirectory (repoPathToOsP repoPath) $ do
        (timeStr, out) <- withTiming $ runGitOut env gitArgs
        putStrLn $ "Search finished: " ++ timeStr
        toText <$> decodeThrowM out
  where
    gitArgs = case env.coreConfig.branches of
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
    Terminal :> es
  ) =>
  Env ->
  Commit ->
  RepoPath ->
  Eff es Bool
doesCommitExist env commit repoPath = do
  (ec, _, _) <-
    PW.withCurrentDirectory (repoPathToOsP repoPath)
      $ runGit env gitArgs
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
    Terminal :> es
  ) =>
  Env ->
  [OsString] ->
  Eff es OsString
runGitOut env args = runProcOut env [osstr|git|] args

runGit ::
  ( HasCallStack,
    Process :> es,
    Terminal :> es
  ) =>
  Env ->
  [OsString] ->
  Eff es (ExitCode, String, String)
runGit env args = runProc env [osstr|git|] args

runGit_ ::
  ( HasCallStack,
    Process :> es,
    Terminal :> es
  ) =>
  Env ->
  [OsString] ->
  Eff es ()
runGit_ env args = do
  (ec, _, err) <- runGit env args
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
    Terminal :> es
  ) =>
  Env ->
  OsString ->
  [OsString] ->
  Eff es OsString
runProcOut env exe args = do
  let exeStr = decodeLenient exe
  (ec, out, err) <- runProc env exe args
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
    Terminal :> es
  ) =>
  Env ->
  OsString ->
  [OsString] ->
  Eff es (ExitCode, String, String)
runProc env exe args = do
  exeStr <- decodeThrowM exe
  argsStrs <- traverse decodeThrowM args

  logDebug env $ do
    let msg =
          mconcat
            [ "runProcess: ",
              L.unwords (exeStr : argsStrs)
            ]
    pure msg

  P.readProcessWithExitCode exeStr argsStrs name
  where
    name = "runProcess"

logDebug ::
  ( HasCallStack,
    Terminal :> es
  ) =>
  Env ->
  Eff es String ->
  Eff es ()
logDebug env mkStr = when env.coreConfig.debug $ do
  s <- mkStr
  putStrLn $ "[Debug]: " ++ s

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
