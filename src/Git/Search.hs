{-# LANGUAGE QuasiQuotes #-}

module Git.Search
  ( -- * Searching
    searchBranch,
    searchCommit,
    searchPullRequest,

    -- * Deleting
    deleteCache,
  )
where

import Data.Aeson qualified as Asn
import Data.Text qualified as T
import Data.Time.Relative (Format (verbosity))
import Data.Time.Relative qualified as Time.Rel
import Effectful.FileSystem.PathReader.Dynamic qualified as PR
import Effectful.FileSystem.PathWriter.Dynamic qualified as PW
import Effectful.HTTP.Client.Static qualified as Eff.Http
import Effectful.Time.Static qualified as Time
import Git.Search.Config (Env)
import Git.Search.Config.Data
  ( DeleteCacheType (DeleteCacheGlobal, DeleteCacheLocal),
    SearchBaseParams,
    SearchBranchParams,
    SearchCommitParams
      ( MkSearchCommitParams,
        baseParams,
        commit
      ),
    SearchPullRequestParams,
  )
import Git.Search.Data
  ( Branch (MkBranch),
    Commit (MkCommit),
    RepoName,
    RepoPath,
    RepoRemoteUri,
    RepoUpdate (RepoUpdateAuto, RepoUpdateClean, RepoUpdateOff),
  )
import Git.Search.Logging qualified as Logging
import Git.Search.Prelude
import Git.Search.Process qualified as Process
import Network.HTTP.Client qualified as Http
import System.OsString (OsChar)
import System.OsString qualified as OsStr

-- | Deletes the cache.
deleteCache ::
  ( HasCallStack,
    PathReader :> es,
    PathWriter :> es,
    Reader Env :> es,
    Terminal :> es,
    Time :> es
  ) =>
  DeleteCacheType ->
  Eff es ()
deleteCache = \case
  DeleteCacheGlobal (MkPath cacheDir) -> do
    Logging.logDebug $ "Deleting cache: " <> decodeLenientt cacheDir

    PW.removePathForcibly cacheDir
  DeleteCacheLocal repoPath -> do
    let repoDir = repoPathToOsP repoPath
        repoDirStr = decodeLenientt repoDir

    Logging.logDebug $ "Deleting cache repo: " <> repoDirStr

    exists <- PR.doesDirectoryExist repoDir
    if exists
      then PW.removePathForcibly repoDir
      else throwText $ "Cached repository does not exist: " <> repoDirStr

-- | Finds the HEAD commit associated to the branch. Then runs searchCommit.
searchBranch ::
  ( HasCallStack,
    PathReader :> es,
    PathWriter :> es,
    Process :> es,
    Reader Env :> es,
    Terminal :> es,
    Time :> es
  ) =>
  SearchBranchParams ->
  Eff es (List Text)
searchBranch params =
  searchCommitHelper
    (params ^. #baseParams)
    (Left $ params ^. #branch)

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
  SearchCommitParams ->
  Eff es (List Text)
searchCommit params =
  searchCommitHelper
    (params ^. #baseParams)
    (Right $ params ^. #commit)

-- | Finds the HEAD commit associated to the PR. Then runs searchCommit.
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
  SearchPullRequestParams ->
  Eff es (List Text)
searchPullRequest params = do
  prToCommit (params ^. #prNum) (params ^. #repoName) >>= \case
    Nothing -> pure []
    Just commit ->
      searchCommit
        $ MkSearchCommitParams
          { baseParams = params ^. #baseParams,
            commit
          }

searchCommitHelper ::
  ( HasCallStack,
    PathReader :> es,
    PathWriter :> es,
    Process :> es,
    Reader Env :> es,
    Terminal :> es,
    Time :> es
  ) =>
  SearchBaseParams ->
  Either Branch Commit ->
  Eff es (List Text)
searchCommitHelper baseParams eBranchCommit = do
  Logging.logDebug $ "Clone destination: " <> repoPathStr

  if baseParams ^. #shouldClone
    then cloneRepo repoPath repoSrc
    else Logging.logInfo $ "Skipping clone/fetch: " <> repoSrcStr

  findBranches eBranchCommit repoPath
  where
    repoPath = baseParams ^. #repoPath
    repoSrc = baseParams ^. #repoSrc

    repoPathOsP = toOsPath $ repoPath ^. #unRepoPath
    repoPathStr = decodeLenientt repoPathOsP

    repoSrcStr = decodeLenientt $ repoSrc ^. #unRepoRemoteUri

-- | Finds the commit associated to the PR, if it exists.
prToCommit ::
  ( HasCallStack,
    Network :> es,
    Reader Env :> es,
    Terminal :> es,
    Time :> es
  ) =>
  -- | PR number.
  Word32 ->
  -- | Repository name.
  RepoName ->
  Eff es (Maybe Commit)
prToCommit prNum repoName = do
  env <- ask @Env

  -- Add auth if given. Primarily used for github CI.
  authHeader <- case env ^. #coreConfig % #auth of
    Nothing -> pure []
    Just auth -> do
      authBs <- encodeUtf8 . pack <$> decodeThrowM auth
      pure [("Authorization", "Bearer " <> authBs)]

  manager <- Eff.Http.newTlsManager
  baseReq <- updateReq authHeader <$> Http.parseRequest baseUrl

  ePr <-
    trySync
      $ Eff.Http.withResponse
        @GitPullRequest
        baseReq
        manager
        (Eff.Http.readResponseJson baseUrl)

  case ePr of
    -- Warn if something went wrong (e.g. PR does not exist).
    --
    -- See NOTE: [Warning on non-existence]
    Left ex -> do
      let msg =
            mconcat
              [ "PR '",
                showt prNum,
                "' not found: ",
                displayExceptiont ex
              ]
      Logging.logWarn msg
      pure Nothing
    Right pr -> do
      case pr ^. #state of
        PullRequestOpen ->
          Logging.logInfo $ "Pull request " <> prStr <> " is currently open."
        PullRequestClosed ->
          if pr ^. #merged
            then
              Logging.logDebug $ "Pull request " <> prStr <> " is merged."
            else
              Logging.logWarn $ "Pull request " <> prStr <> " is closed but unmerged."

      let commitStr = T.strip $ pr ^. #head % #sha

      Logging.logDebug $ "Found commit (" <> showt (pr ^. #commits) <> "): " <> commitStr

      Just . MkCommit <$> encodeThrowM (unpack commitStr)
  where
    prStr = showt prNum

    baseUrl =
      unpack
        $ mconcat
          [ "https://api.github.com/repos/",
            decodeLenientt $ repoName ^. #unRepoName,
            "/pulls/",
            prStr
          ]

    updateReq hs r =
      r
        { Http.requestHeaders =
            [ ("Accept", "application/json;charset=utf-8,application/json"),
              -- Need this agent so github does not block us.
              ("User-Agent", "Mozilla/5.0 (Windows NT 10.0; Win64; x64)")
            ]
              ++ hs
        }

data GitPullRequest = MkGitPullRequest
  { -- | Number of commits.
    commits :: Word32,
    -- | HEAD commit.
    head :: GitCommit,
    -- | Merged status.
    merged :: Bool,
    -- | State.
    state :: PullRequestState
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

data PullRequestState
  = PullRequestClosed
  | PullRequestOpen

instance FromJSON PullRequestState where
  parseJSON = Asn.withText "PullRequestState" $ \case
    "open" -> pure PullRequestOpen
    "closed" -> pure PullRequestClosed
    other -> fail $ unpack $ "Unrecognized state: " <> other

newtype GitCommit = MkGitCommit
  { sha :: Text
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

-- | Clones the repo or runs fetch.
cloneRepo ::
  ( HasCallStack,
    PathReader :> es,
    PathWriter :> es,
    Process :> es,
    Reader Env :> es,
    Terminal :> es,
    Time :> es
  ) =>
  -- | Repo file-system path.
  RepoPath ->
  -- | Full repo remote uri.
  RepoRemoteUri ->
  Eff es ()
cloneRepo repoPath repoSrc = do
  env <- ask @Env

  exists <- PR.doesDirectoryExist repoPathOsP

  if exists
    then do
      case env ^. #coreConfig % #update of
        -- 1. Repo exists and normal behavior: fetch.
        RepoUpdateAuto -> runFetch
        -- 2. Repo exists but 'clean' is active: delete and clone.
        RepoUpdateClean -> do
          PW.removeDirectoryRecursive repoPathOsP
          runClone
        -- 3. Repo exists but 'off' is active: skip update.
        RepoUpdateOff -> Logging.logDebug "Skipping fetch."
    else
      -- 4. Repo does not exist: clone.
      runClone
  where
    runClone = do
      Logging.logInfo $ "Cloning " <> repoSrcStr <> "..."
      timeStr <- withTiming_ $ Process.runGit cloneArgs
      Logging.logInfo $ "Clone finished: " <> timeStr

    runFetch = do
      env <- ask @Env
      Logging.logInfo $ "Fetching " <> repoSrcStr <> "..."

      timeStr <- PW.withCurrentDirectory (repoPathToOsP repoPath) $ do
        withTiming_
          . Process.runGit
          . fetchArgs
          . view (#coreConfig % #repo % #remoteName)
          $ env
      Logging.logInfo $ "Fetch finished: " <> timeStr

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
        repoSrc ^. #unRepoRemoteUri,
        repoPathOsP
      ]

    fetchArgs remoteName =
      [ [osstr|fetch|],
        remoteName ^. #unRepoRemoteName,
        [osstr|--prune|]
      ]

    repoPathOsP = toOsPath $ repoPath ^. #unRepoPath
    repoSrcStr = decodeLenientt $ repoSrc ^. #unRepoRemoteUri

-- | Finds all branches containing the given branch HEAD or commit.
findBranches ::
  ( HasCallStack,
    PathWriter :> es,
    Process :> es,
    Reader Env :> es,
    Terminal :> es,
    Time :> es
  ) =>
  Either Branch Commit ->
  RepoPath ->
  Eff es (List Text)
findBranches eBranchCommit repoPath = do
  case eBranchCommit of
    Left b ->
      branchToCommit b repoPath >>= \case
        Nothing -> pure []
        Just c -> onCommit c
    Right c -> onCommit c
  where
    toText = fmap T.strip . T.lines . pack

    onCommit commit = do
      env <- ask @Env

      let gitDefArgs =
            [ [osstr|branch|],
              [osstr|-r|],
              [osstr|--contains|],
              commit ^. #unCommit
            ]

          gitArgs branches = case branches of
            [] -> gitDefArgs
            bs@(_ : _) -> gitBranchArgs bs

          gitBranchArgs bs = gitDefArgs ++ ([osstr|--list|] : bs)

          hashStr = decodeLenientt $ commit ^. #unCommit

      Logging.logInfo $ "Searching for commit " <> hashStr <> "..."

      commitExists commit repoPath >>= \case
        False -> pure []
        True ->
          PW.withCurrentDirectory (repoPathToOsP repoPath) $ do
            (timeStr, out) <-
              withTiming
                . Process.runGitOut
                . gitArgs
                . view (#coreConfig % #repo % #branches)
                $ env
            Logging.logInfo $ "Search finished: " <> timeStr
            toText <$> decodeThrowM out

branchToCommit ::
  ( HasCallStack,
    PathWriter :> es,
    Process :> es,
    Reader Env :> es,
    Terminal :> es,
    Time :> es
  ) =>
  Branch ->
  RepoPath ->
  Eff es (Maybe Commit)
branchToCommit branch repoPath = do
  -- Unless the given branch has a '/' in it, we need to qualify it with
  -- the remote name (e.g. 'origin').
  let branchOsStr = branch ^. #unBranch

  qualBranch <-
    if OsStr.elem slashChar branchOsStr
      then pure branchOsStr
      else prependRemoteName branchOsStr

  let qualBranchTxt = decodeLenientt qualBranch

      gitArgs =
        [ [osstr|rev-parse|],
          qualBranch
        ]

  Logging.logInfo $ "Searching for branch " <> qualBranchTxt <> "..."

  branchExists (MkBranch qualBranch) repoPath >>= \case
    False -> pure Nothing
    True ->
      fmap (Just . MkCommit)
        $ PW.withCurrentDirectory (repoPathToOsP repoPath)
        $ Process.runGitOut gitArgs

branchExists ::
  ( HasCallStack,
    PathWriter :> es,
    Process :> es,
    Reader Env :> es,
    Terminal :> es,
    Time :> es
  ) =>
  Branch ->
  RepoPath ->
  Eff es Bool
branchExists branch = gitObjExists "Branch" (branch ^. #unBranch)

commitExists ::
  ( HasCallStack,
    PathWriter :> es,
    Process :> es,
    Reader Env :> es,
    Terminal :> es,
    Time :> es
  ) =>
  Commit ->
  RepoPath ->
  Eff es Bool
commitExists commit = gitObjExists "Commit" (commit ^. #unCommit)

-- NOTE: [Warning on non-existence]
--
-- We choose to issue warnings and return an empty list, rather than erroring,
-- when certain objects are not found. For instance:
--
-- - Branch not found
-- - Commit
-- - PR not found
--
-- While it is plausible that a user typing in a bad branch or commit might
-- deserve an error, there are at least two situations where this could be
-- undesirable.
--
-- 1. Some non-existence can reasonably be expected.
--
--    For example, suppose we are searching for an PR from a fork that happens
--    to be unmerged. It is likely that its HEAD commit will not exist in our
--    repo at all (since it is on a fork), which will mean our existence check
--    'git cat-file' will fail.
--
--    But this really isn't any different from checking an unmerged PR that
--    is /not/ on a fork, where the commit will in fact exist in our repo.
--
--    Both cases are really the same i.e. should report no branches found.
--
-- 2. These errors are especially awkward with the search-defaults command.
--    That is, a single error will take down the entire sequence, which is
--    okay for something pathological, but shouldn't happen for something
--    relatively normal.
--
-- For these reasons, we should not error for 'not found' situations, and
-- instead log it and return no matches.

-- | Returns true iff the git object exists. Logs a warning if not.
-- See NOTE: [Warning on non-existence].
gitObjExists ::
  ( HasCallStack,
    PathWriter :> es,
    Process :> es,
    Reader Env :> es,
    Terminal :> es,
    Time :> es
  ) =>
  Text ->
  OsString ->
  RepoPath ->
  Eff es Bool
gitObjExists tyStr obj repoPath = do
  (ec, _, _) <-
    PW.withCurrentDirectory repoOsP $ Process.runGit gitArgs
  case ec of
    ExitSuccess -> pure True
    ExitFailure _ -> do
      let msg =
            mconcat
              [ tyStr,
                " '",
                decodeLenientt obj,
                "' not found."
              ]
      Logging.logWarn msg
      pure False
  where
    repoOsP = repoPathToOsP repoPath

    gitArgs =
      [ [osstr|cat-file|],
        [osstr|-e|],
        obj
      ]

withTiming ::
  ( HasCallStack,
    Time :> es
  ) =>
  Eff es a ->
  Eff es (Text, a)
withTiming m = do
  (ts, r) <- Time.withTiming m
  let tsStr =
        pack
          . Time.Rel.formatRelativeTime fmt
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

withTiming_ :: (HasCallStack, Time :> es) => Eff es a -> Eff es Text
withTiming_ = fmap fst . withTiming

repoPathToOsP :: RepoPath -> OsPath
repoPathToOsP = toOsPath . view #unRepoPath

prependRemoteName ::
  (Reader Env :> es) =>
  OsString ->
  Eff es OsString
prependRemoteName osStr = do
  remoteName <- asks @Env (view (#coreConfig % #repo % #remoteName))

  pure
    . (<> OsStr.cons slashChar osStr)
    . view #unRepoRemoteName
    $ remoteName

slashChar :: OsChar
slashChar = OsStr.unsafeFromChar '/'
