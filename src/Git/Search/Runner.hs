{-# LANGUAGE QuasiQuotes #-}

module Git.Search.Runner
  ( runSearch,
  )
where

import Control.Exception.Utils qualified as Ex.Utils
import Effectful.Concurrent qualified as CC
import Effectful.Concurrent.Async qualified as Async
import Effectful.Exception qualified as Ex
import Effectful.FileSystem.FileReader.Static qualified as FR
import Effectful.FileSystem.HandleReader.Static qualified as HR
import Effectful.FileSystem.HandleWriter.Dynamic qualified as HW
import Effectful.FileSystem.PathReader.Static qualified as PR
import Git.Search qualified
import Git.Search.Config qualified as Config
import Git.Search.Config.Args (Args)
import Git.Search.Config.Args qualified
import Git.Search.Config.Data
  ( Command (SearchCommit),
    WithDisabled (Disabled, With),
  )
import Git.Search.Config.Toml (Toml)
import Git.Search.Prelude
import System.IO qualified as IO
import TOML qualified

runSearch ::
  ( Concurrent :> es,
    FileReader :> es,
    HasCallStack,
    HandleReader :> es,
    HandleWriter :> es,
    Optparse :> es,
    PathReader :> es,
    PathWriter :> es,
    Process :> es,
    Terminal :> es,
    Time :> es
  ) =>
  Eff es ()
runSearch = withHiddenInput $ do
  args <- Git.Search.Config.Args.getArgs
  mToml <- getToml args

  (env, cmd) <- Config.toEnv args mToml

  let cmdAction = case cmd of
        SearchCommit cmdArgs -> Git.Search.searchCommit env cmdArgs

  branches <- race' cmdAction drainStdinLoop

  case branches of
    [] -> putStrLn "No branches found."
    bs@(_ : _) -> do
      let numBranches = length bs
          formatted = mconcat $ fmap ("\n - " <>) bs
      putStrLn
        $ mconcat
          [ "Found ",
            show numBranches,
            " branches:",
            unpack formatted
          ]

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

race' :: (Concurrent :> es) => Eff es a -> Eff es a -> Eff es a
race' mx my = Async.race mx my <&> either id id

getXdgConfig :: (HasCallStack, PathReader :> es) => Eff es OsPath
getXdgConfig = PR.getXdgConfig [osp|git-search|]
