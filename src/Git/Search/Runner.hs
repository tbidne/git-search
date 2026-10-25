{-# LANGUAGE QuasiQuotes #-}

module Git.Search.Runner
  ( -- * Main
    runSearch,

    -- * Misc
    getEnv,
  )
where

import Control.Exception.Utils qualified as Ex.Utils
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
import Git.Search.Config.Phase (ConfigPhase (ConfigPhaseEnv))
import Git.Search.Config.Toml (Toml)
import Git.Search.Config.WithDisabled (WithDisabled (Disabled, With))
import Git.Search.Logging qualified as Logging
import Git.Search.Logging.Data (LogLevel (LogLevelCommand))
import Git.Search.Prelude
import System.IO qualified as IO
import TOML qualified

runSearch ::
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

  let cmdAction =
        for_ envCmds $ \(env, cmd, mLog) -> do
          runReader env $ do
            for_ mLog (uncurry Logging.logWithLevel)
            runCmd cmd

  Async.race_ cmdAction drainStdinLoop
  where
    runCmd = \case
      DeleteCache cmdArgs -> Git.Search.deleteCache cmdArgs
      SearchCommit cmdArgs -> Git.Search.searchCommit cmdArgs >>= printBranches
      SearchPullRequest cmdArgs -> Git.Search.searchPullRequest cmdArgs >>= printBranches

    printBranches branches = case branches of
      [] -> Logging.logSuccess "No branches found.\n"
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
              show numBranches,
              suffix,
              unpack formatted,
              "\n"
            ]

getEnv ::
  ( FileReader :> es,
    HasCallStack,
    Optparse :> es,
    PathReader :> es,
    PathWriter :> es
  ) =>
  Eff es [(Env, Command ConfigPhaseEnv, Maybe (LogLevel, String))]
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
      searchDefaults = fromMaybe [] (mToml ^? (_Just % #searchDefaults))

  case cmd1 of
    DeleteCache _ -> liftOne <$> Config.toEnv args mToml
    SearchCommit _ -> liftOne <$> Config.toEnv args mToml
    SearchDefaults () -> do
      for searchDefaults $ \(eCmd, mName, mDesc) -> do
        -- Create the actual command to run.
        let desc = unpack $ maybe "" (": " <>) mDesc
            (tmpCmd, msg) = case eCmd of
              Left commit ->
                let commitStr = decodeLenient $ commit ^. #unCommit
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
                          show pr,
                          ")",
                          desc
                        ]
                 in (SearchPullRequest pr, logMsg)

        -- Create the env and final cmd, setting the repo name if necessary.
        (\(cmd, env) -> (cmd, env, Just (LogLevelCommand, msg)))
          <$> case mName of
            Nothing -> Config.toEnv (setCmd tmpCmd args) mToml
            Just name -> Config.toEnv (setCmd tmpCmd $ setName name args) mToml
    SearchPullRequest _ -> liftOne <$> Config.toEnv args mToml
  where
    setCmd = set' #command
    setName = set' (#coreConfig % #repo % #name) . (Just . With)

    liftOne (env, cmd) = [(env, cmd, Nothing)]

getToml ::
  ( HasCallStack,
    FileReader :> es,
    PathReader :> es
  ) =>
  Args ->
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
