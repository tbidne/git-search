{-# LANGUAGE QuasiQuotes #-}

module Git.Search.Runner
  ( -- * Main
    runSearch,

    -- * Misc
    getEnv,
  )
where

import Control.Exception.Utils qualified as Ex.Utils
import Data.List qualified as L
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Tuple (uncurry)
import Effectful.Concurrent qualified as CC
import Effectful.Concurrent.Async qualified as Async
import Effectful.Exception qualified as Ex
import Effectful.FileSystem.FileReader.Static qualified as FR
import Effectful.FileSystem.HandleReader.Static qualified as HR
import Effectful.FileSystem.HandleWriter.Dynamic qualified as HW
import Effectful.FileSystem.PathReader.Dynamic qualified as PR
import Git.Search qualified
import Git.Search.Config (Env)
import Git.Search.Config qualified as Config
import Git.Search.Config.Args (Args)
import Git.Search.Config.Args qualified
import Git.Search.Config.Data
  ( Command
      ( DeleteCache,
        SearchCommit,
        SearchDefaults,
        SearchPullRequest
      ),
  )
import Git.Search.Config.Phase (ConfigPhase (ConfigPhaseArgs1, ConfigPhaseEnv))
import Git.Search.Config.Toml (Toml)
import Git.Search.Config.WithDisabled (WithDisabled (Disabled, With))
import Git.Search.Data (RepoPath, RepoRemoteUri)
import Git.Search.Logging qualified as Logging
import Git.Search.Logging.Data (LogLevel (LogLevelCommand))
import Git.Search.Prelude
import Optics.Core (LabelOptic')
import System.IO qualified as IO
import TOML qualified

runSearch ::
  forall es.
  ( Concurrent :> es,
    FileReader :> es,
    HasCallStack,
    HandleReader :> es,
    HandleWriter :> es,
    Network :> es,
    Optparse :> es,
    PathReader :> es,
    PathWriter :> es,
    Process :> es,
    Terminal :> es,
    Time :> es
  ) =>
  Eff es ()
runSearch = withHiddenInput $ do
  envCmds <- getEnv

  let envCmdsFinal = stripCloneDupes envCmds
      numCmds = length envCmdsFinal

      envCmdsFinalIdx =
        Seq.zip
          (Seq.fromList [1, 2 .. numCmds])
          envCmdsFinal

      cmdAction =
        for_ envCmdsFinalIdx $ \(idx, (env, cmd, mLog)) -> do
          runReader env $ do
            for_ mLog (uncurry Logging.logWithLevel)
            runCmd (numCmds == idx) cmd

  Async.race_ cmdAction drainStdinLoop
  where
    -- Need the type signature, otherwise the extra Bool param infers
    -- Command p, and GHC cannot eliminate the SearchDefaults pattern.
    runCmd :: Bool -> Command ConfigPhaseEnv -> Eff (Reader Env : es) ()
    runCmd finalCmd = \case
      DeleteCache cmdArgs -> Git.Search.deleteCache cmdArgs
      SearchCommit cmdArgs -> Git.Search.searchCommit cmdArgs >>= printBranches finalCmd
      SearchPullRequest cmdArgs -> Git.Search.searchPullRequest cmdArgs >>= printBranches finalCmd

    -- Remove duplicate clones i.e. if we have multiple commands
    -- (search-defaults) that will perform the same clone step, then we
    -- should only perform it once, to save work.
    stripCloneDupes :: Seq EnvCmd -> Seq EnvCmd
    stripCloneDupes = fst . L.foldl' stripCloneDupe (Seq.empty, Set.empty)

    -- A "duplicate clone" is a SearchCommit/SearchPullRequest command that
    -- has the same RepoPath (local file-system path) and RepoRemotUri
    -- (remote source). Note that no consideration is given to different
    -- remotes sharing the same local path i.e. you are on your own.
    stripCloneDupe :: (Seq EnvCmd, Set RepoProv) -> EnvCmd -> (Seq EnvCmd, Set RepoProv)
    stripCloneDupe (envCmds, foundRepos) envCmd@(env, cmd, mLog) = case cmd of
      DeleteCache {} -> (envCmds :|> envCmd, foundRepos)
      SearchCommit params ->
        let (foundRepos', params') = handleCloneDupes foundRepos params
         in (envCmds :|> (env, SearchCommit params', mLog), foundRepos')
      SearchPullRequest params ->
        let (foundRepos', params') = handleCloneDupes foundRepos params
         in (envCmds :|> (env, SearchPullRequest params', mLog), foundRepos')

    -- Check the command params for a duplicate repo.
    handleCloneDupes ::
      forall x.
      ( LabelOptic' "repoPath" A_Lens x RepoPath,
        LabelOptic' "repoSrc" A_Lens x RepoRemoteUri,
        LabelOptic' "shouldClone" A_Lens x Bool
      ) =>
      Set RepoProv ->
      x ->
      (Set RepoProv, x)
    handleCloneDupes foundRepos params =
      let repoParams = (params ^. #repoPath, params ^. #repoSrc)
       in if Set.member repoParams foundRepos
            -- 1. We found a duplicate. Set the command's clone to false.
            then (foundRepos, set' #shouldClone False params)
            -- 2. No duplicates, proceed normally, adding the newly
            --   found repo.
            else (Set.insert repoParams foundRepos, params)

    printBranches finalCmd branches = do
      -- If this is not the final command, append a newline.
      let endLine = if finalCmd then "" else "\n"
      case branches of
        [] -> Logging.logSuccess $ "No branches found." <> endLine
        bs@(_ : _) -> do
          let numBranches = length bs
              formatted = mconcat $ fmap ("\n - " <>) bs
              suffix =
                if numBranches == 1
                  then " branch:"
                  else " branches:"

          Logging.logSuccess
            $ mconcat
              [ "Found ",
                showt numBranches,
                suffix,
                formatted,
                endLine
              ]

type EnvCmd = (Env, Command ConfigPhaseEnv, Maybe (LogLevel, Text))

type RepoProv = (RepoPath, RepoRemoteUri)

getEnv ::
  ( FileReader :> es,
    HasCallStack,
    Optparse :> es,
    PathReader :> es,
    PathWriter :> es
  ) =>
  Eff es (Seq EnvCmd)
getEnv = do
  args <- Git.Search.Config.Args.getArgs
  mToml <- getToml args

  -- NOTE: [Env and Cmd creation]
  --
  -- To accommodate SearchDefaults, we create the final cmd and env in multiple
  -- stages.
  --
  -- That is, the Args and Toml are both required to create the runtime Env
  -- and final command that is run. Furthermore, Args needs its repoName
  -- field set in order for the Env to be correctly set.
  --
  -- This is a problem for the SearchDefaults command. That is, SearchDefaults
  -- runs a series of SearchCommit and SearchPullRequest commands, based on
  -- the toml field. Note that each command can set its own repoName.
  --
  -- But this means if we have already created the Env from the original Args
  -- and Toml, it will not be correctly set for each command, since each
  -- command may have a different repoName from Args.
  --
  -- Therefore, we first pattern match on the args' command. For normal
  -- commands, create the Env and final Cmd as usual and run them.
  --
  -- For SearchDefaults, we run each command from the toml sequentially. If
  -- the command has its repo name set, we set it on Args. We then create
  -- the Env and final Command, and run it. This way the Env is correct.
  --
  -- This does mean we pattern match on the command several times:
  --
  --   - 1: Determine how to construct env / final cmd.
  --   - 2: In Config.toEnv, to actually construct env / final cmd (could be
  --        consolidated to remove redundancy).
  --   - 3: Final time here, when running the actual command.
  let cmd1 = args ^. #command
      searchDefaults = fromMaybe Seq.empty (mToml ^? (_Just % #searchDefaults))

  case cmd1 of
    DeleteCache params ->
      liftOne
        <$> Config.toEnv
          (set' #command (DeleteCache params) args)
          mToml
    SearchCommit params ->
      liftOne
        <$> Config.toEnv
          (set' #command (SearchCommit params) args)
          mToml
    SearchDefaults () -> do
      for searchDefaults $ \(eCmd, repoName, mDesc) -> do
        -- Create the actual command to run.
        let desc = maybe "" (": " <>) mDesc
            (tmpCmd, msg) = case eCmd of
              Left commit ->
                let commitStr = pack $ decodeLenient $ commit ^. #unCommit
                    logMsg =
                      mconcat
                        [ "Running search-commit (",
                          commitStr,
                          ")",
                          desc
                        ]
                 in (SearchCommit commit, logMsg)
              Right pr ->
                let logMsg =
                      mconcat
                        [ "Running search-pr (",
                          showt pr,
                          ")",
                          desc
                        ]
                 in (SearchPullRequest pr, logMsg)

        -- Create the env and final cmd, setting the repo name.
        (\(cmd, env) -> (cmd, env, Just (LogLevelCommand, msg)))
          <$> Config.toEnv (setCmd tmpCmd $ setName repoName args) mToml
    SearchPullRequest params ->
      liftOne
        <$> Config.toEnv
          (set' #command (SearchPullRequest params) args)
          mToml
  where
    setCmd = set' #command

    -- Always overwrite name with search-defaults.
    -- See NOTE: [SearchDefault name]
    setName = set' (#coreConfig % #repo % #name) . (Just . With)

    liftOne (env, cmd) = Seq.singleton (env, cmd, Nothing)

getToml ::
  ( HasCallStack,
    FileReader :> es,
    PathReader :> es
  ) =>
  Args ConfigPhaseArgs1 ->
  Eff es (Maybe Toml)
getToml args = do
  case args.config of
    Just (With p) -> Just <$> readToml p
    Just Disabled -> pure Nothing
    Nothing -> do
      xdg <- getXdgConfig
      let cfg = xdg </> [osp|config.toml|]
      exists <- PR.doesFileExist cfg
      if exists
        then Just <$> readToml cfg
        else pure Nothing
  where
    readToml path = do
      contents <- FR.readFileUtf8ThrowM path
      case TOML.decode contents of
        Right t -> pure t
        Left err -> throwM err

withHiddenInput ::
  ( HasCallStack,
    HandleReader :> es,
    HandleWriter :> es
  ) =>
  Eff es a ->
  Eff es a
withHiddenInput m = Ex.bracket hideInput unhideInput (const m)
  where
    -- Note that this may not work on windows, if we ever want that.
    --
    -- - https://stackoverflow.com/questions/15848975/preventing-input-characters-appearing-in-terminal
    -- - https://hackage.haskell.org/package/echo
    hideInput = do
      buffMode <- HR.hGetBuffering IO.stdin
      echoMode <- HR.hGetEcho IO.stdin
      HW.hSetBuffering IO.stdin HW.NoBuffering

      -- Needed in case another command runs this and tries to read the output.
      -- HW.hSetBuffering IO.stderr IO.LineBuffering
      HW.hSetBuffering IO.stdout IO.LineBuffering

      HW.hSetEcho IO.stdin False
      pure (buffMode, echoMode)

    unhideInput (buffMode, echoMode) = do
      HW.hSetBuffering IO.stdin buffMode
      HW.hSetEcho IO.stdin echoMode

drainStdinLoop ::
  forall es void.
  ( Concurrent :> es,
    HasCallStack,
    HandleReader :> es
  ) =>
  Eff es void
drainStdinLoop = go
  where
    go = forever $ do
      drainStdin
      -- 60_000_000 microseconds <=> 60 seconds
      CC.threadDelay 60_000_000

drainStdin :: (HasCallStack, HandleReader :> es) => Eff es ()
drainStdin =
  void
    $ Ex.Utils.trySync
    $ HR.hIsClosed IO.stdin
    >>= \case
      True -> pure ()
      False ->
        HR.hIsReadable IO.stdin >>= \case
          False -> pure ()
          True -> void $ HR.hGetNonBlocking IO.stdin 1_000

getXdgConfig :: (HasCallStack, PathReader :> es) => Eff es OsPath
getXdgConfig = PR.getXdgConfig [osp|git-search|]
