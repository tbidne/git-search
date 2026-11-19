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
  )
import Git.Search.Logging qualified as Logging
import Git.Search.Prelude
import Git.Search.Process qualified as Process
import Network.HTTP.Client qualified as Http
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

-- | Returns a list of branches matching the search criteria.
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
  commit <- prToCommit (params ^. #prNum) (params ^. #repoName)
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

prToCommit ::
  ( HasCallStack,
    Network :> es,
    Reader Env :> es,
    Terminal :> es,
    Time :> es
  ) =>
  Word32 ->
  RepoName ->
  Eff es Commit
prToCommit prNum repoName = do
  env <- ask @Env

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

  pr <- case ePr of
    Left ex -> do
      let msg =
            mconcat
              [ "Could not find pr ",
                showt prNum,
                ": ",
                displayExceptiont ex
              ]
      Logging.logError msg
      throwText msg
    Right x -> pure x

  case pr ^. #state of
    PullRequestOpen ->
      Logging.logInfo $ "Pull request " <> prStr <> " is currently open."
    PullRequestClosed ->
      if pr ^. #merged
        then
          Logging.logDebug $ "Pull request " <> prStr <> " is merged."
        else
          Logging.logWarn $ "Pull request " <> prStr <> " is closed but unmerged."

  let commitStr = pr ^. #head % #sha

  Logging.logDebug $ "Found commit (" <> showt (pr ^. #commits) <> "): " <> commitStr

  MkCommit <$> encodeThrowM (unpack commitStr)
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

  exists <- PR.doesDirectoryExist repoPathOsP

  if exists
    then do
      if env ^. #coreConfig % #clean
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
  env <- ask @Env

  commit <- case eBranchCommit of
    Left b -> branchToCommit b repoPath
    Right c -> pure c

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

  verifyCommitExists commit repoPath

  PW.withCurrentDirectory (repoPathToOsP repoPath) $ do
    (timeStr, out) <-
      withTiming
        . Process.runGitOut
        . gitArgs
        . view (#coreConfig % #repo % #branches)
        $ env
    Logging.logInfo $ "Search finished: " <> timeStr
    toText <$> decodeThrowM out
  where
    toText = fmap T.strip . T.lines . pack

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
  Eff es Commit
branchToCommit branch repoPath = do
  remoteName <- asks @Env (view (#coreConfig % #repo % #remoteName))

  -- Unless the given branch has a '/' in it, we need to qualify it with
  -- the remote name (e.g. 'origin').
  let branchOsStr = branch ^. #unBranch

      qualBranch =
        if OsStr.elem slashChar branchOsStr
          then branchOsStr
          else
            remoteName
              ^. #unRepoRemoteName
              <> OsStr.cons slashChar branchOsStr

      qualBranchTxt = decodeLenientt qualBranch

      gitArgs =
        [ [osstr|rev-parse|],
          qualBranch
        ]

  Logging.logInfo $ "Searching for branch " <> qualBranchTxt <> "..."

  verifyBranchExists (MkBranch qualBranch) repoPath

  PW.withCurrentDirectory (repoPathToOsP repoPath) $ do
    fmap MkCommit
      . stripOsStrThrowM
      <=< Process.runGitOut
      $ gitArgs
  where
    slashChar = OsStr.unsafeFromChar '/'

verifyBranchExists ::
  ( HasCallStack,
    PathWriter :> es,
    Process :> es,
    Reader Env :> es,
    Terminal :> es,
    Time :> es
  ) =>
  Branch ->
  RepoPath ->
  Eff es ()
verifyBranchExists branch = verifyGitObjExists "Branch" (branch ^. #unBranch)

verifyCommitExists ::
  ( HasCallStack,
    PathWriter :> es,
    Process :> es,
    Reader Env :> es,
    Terminal :> es,
    Time :> es
  ) =>
  Commit ->
  RepoPath ->
  Eff es ()
verifyCommitExists commit = verifyGitObjExists "Commit" (commit ^. #unCommit)

verifyGitObjExists ::
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
  Eff es ()
verifyGitObjExists tyStr obj repoPath = do
  (ec, _, _) <-
    PW.withCurrentDirectory repoOsP $ Process.runGit gitArgs
  case ec of
    ExitSuccess -> pure ()
    ExitFailure _ -> do
      let msg =
            mconcat
              [ tyStr,
                " '",
                decodeLenientt obj,
                "' does not exist."
              ]
      Logging.logError msg
      throwText msg
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

stripOsStrThrowM ::
  (HasCallStack) =>
  OsString ->
  Eff es OsString
stripOsStrThrowM str = do
  txt <- pack <$> decodeThrowM str
  let stripped = T.strip txt
  encodeThrowM $ unpack stripped
