{-# LANGUAGE QuasiQuotes #-}

module Git.Search
  ( searchPrint,
    search,
  )
where

import Control.Exception.Utils qualified as Ex.Utils
import Control.Monad (when)
import Data.List qualified as L
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Relative (Format (verbosity))
import Data.Time.Relative qualified as Time.Rel
import Effectful (Eff, (:>))
import Effectful.FileSystem.PathReader.Static (PathReader)
import Effectful.FileSystem.PathReader.Static qualified as PR
import Effectful.FileSystem.PathWriter.Static (PathWriter)
import Effectful.FileSystem.PathWriter.Static qualified as PW
import Effectful.Process (Process)
import Effectful.Process qualified as P
import Effectful.Terminal.Dynamic (Terminal)
import Effectful.Terminal.Dynamic qualified as Term
import Effectful.Time.Static (Time)
import Effectful.Time.Static qualified as Time
import FileSystem.OsPath qualified as FS.OsP
import FileSystem.OsString (OsString, osstr)
import FileSystem.OsString qualified as FS.OsStr
import FileSystem.Path qualified as FS.Path
import GHC.Stack.Types (HasCallStack)
import Git.Search.Config
  ( Args,
    Config (clean, commit, debug, repo),
    Env,
    RepoEnv (path, src),
  )
import Git.Search.Config qualified as Config
import System.Exit (ExitCode (ExitFailure, ExitSuccess))

-- | Prints a list of branches matching the search criteria.
searchPrint ::
  ( HasCallStack,
    PathReader :> es,
    PathWriter :> es,
    Process :> es,
    Terminal :> es,
    Time :> es
  ) =>
  Args ->
  Eff es ()
searchPrint args = do
  branches <- search args

  case branches of
    [] -> Term.putStrLn "No branches found."
    bs@(_ : _) -> do
      let formatted = mconcat $ fmap ("\n - " <>) bs
      Term.putStrLn $
        mconcat
          [ "Found branches:",
            T.unpack formatted
          ]

-- | Returns a list of branches matching the search criteria.
search ::
  ( HasCallStack,
    PathReader :> es,
    PathWriter :> es,
    Process :> es,
    Terminal :> es,
    Time :> es
  ) =>
  Args ->
  Eff es [Text]
search args = do
  env <- Config.toEnv args

  cloneRepo env
  findBranches env

cloneRepo ::
  ( HasCallStack,
    PathReader :> es,
    PathWriter :> es,
    Process :> es,
    Terminal :> es,
    Time :> es
  ) =>
  Env ->
  Eff es ()
cloneRepo env = do
  when env.debug $ do
    Term.putStrLn $ "Clone destination: " <> repoPathStr

  exists <- PR.doesDirectoryExist repoPathOsP

  if exists
    then do
      if env.clean
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
      Term.putStrLn $ "Cloning " ++ repoSrcStr ++ "..."
      timeStr <- withTiming_ $ runGit_ env cloneArgs
      Term.putStrLn $ "Clone finished: " ++ timeStr

    runFetch = do
      Term.putStrLn $ "Fetching " ++ repoSrcStr ++ "..."
      timeStr <- PW.withCurrentDirectory (FS.Path.toOsPath env.repo.path) $ do
        withTiming_ $ runGit_ env fetchArgs
      Term.putStrLn $ "Fetch finished: " ++ timeStr

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
        env.repo.src,
        repoPathOsP
      ]

    fetchArgs =
      [ [osstr|fetch|],
        [osstr|--prune|]
      ]

    repoPathOsP = FS.Path.toOsPath env.repo.path
    repoPathStr = FS.OsP.decodeLenient repoPathOsP
    repoSrcStr = FS.OsP.decodeLenient env.repo.src

findBranches ::
  ( HasCallStack,
    PathWriter :> es,
    Process :> es,
    Terminal :> es,
    Time :> es
  ) =>
  Env ->
  Eff es [Text]
findBranches env = do
  Term.putStrLn $ "Searching for hash " ++ hashStr ++ "..."

  commitExists <- doesCommitExist env

  if not commitExists
    then do
      Term.putStrLn "Commit does not exist."
      pure []
    else do
      PW.withCurrentDirectory (FS.Path.toOsPath env.repo.path) $ do
        (timeStr, out) <- withTiming $ runGitOut env gitArgs
        Term.putStrLn $ "Search finished: " ++ timeStr
        toText <$> FS.OsStr.decodeThrowM out
  where
    gitArgs =
      [ [osstr|branch|],
        [osstr|-r|],
        [osstr|--contains|],
        env.commit
      ]

    toText = fmap T.strip . T.lines . T.pack

    hashStr = FS.OsStr.decodeLenient env.commit

doesCommitExist ::
  ( HasCallStack,
    PathWriter :> es,
    Process :> es,
    Terminal :> es
  ) =>
  Env ->
  Eff es Bool
doesCommitExist env = do
  (ec, _, _) <-
    PW.withCurrentDirectory (FS.Path.toOsPath env.repo.path) $
      runGit env gitArgs
  pure $ case ec of
    ExitSuccess -> True
    ExitFailure _ -> False
  where
    gitArgs =
      [ [osstr|cat-file|],
        [osstr|-e|],
        env.commit
      ]

runGitOut ::
  ( HasCallStack,
    Process :> es,
    Terminal :> es
  ) =>
  Env ->
  [OsString] ->
  Eff es OsString
runGitOut env args = runProcessOut env [osstr|git|] args

runGit ::
  ( HasCallStack,
    Process :> es,
    Terminal :> es
  ) =>
  Env ->
  [OsString] ->
  Eff es (ExitCode, String, String)
runGit env args = runProcess env [osstr|git|] args

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
      let argsStr = L.unwords $ fmap FS.OsStr.decodeLenient args
      Ex.Utils.throwString $
        T.unpack $
          mconcat
            [ "Error running git with args '",
              T.pack argsStr,
              "': ",
              T.pack err
            ]
    ExitSuccess -> pure ()

runProcessOut ::
  ( HasCallStack,
    Process :> es,
    Terminal :> es
  ) =>
  Env ->
  OsString ->
  [OsString] ->
  Eff es OsString
runProcessOut env exe args = do
  let exeStr = FS.OsStr.decodeLenient exe
  (ec, out, err) <- runProcess env exe args
  case ec of
    ExitFailure _ -> do
      let argsStr = L.unwords $ fmap FS.OsStr.decodeLenient args
      Ex.Utils.throwString $
        T.unpack $
          mconcat
            [ "Error running process '",
              T.pack exeStr,
              "' with args '",
              T.pack argsStr,
              "': ",
              T.pack err
            ]
    ExitSuccess -> FS.OsStr.encodeThrowM out

runProcess ::
  ( HasCallStack,
    Process :> es,
    Terminal :> es
  ) =>
  Env ->
  OsString ->
  [OsString] ->
  Eff es (ExitCode, String, String)
runProcess env exe args = do
  exeStr <- FS.OsStr.decodeThrowM exe
  argsStrs <- traverse FS.OsStr.decodeThrowM args

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
logDebug env mkStr = when env.debug $ do
  s <- mkStr
  Term.putStrLn $ "[Debug]: " ++ s

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
