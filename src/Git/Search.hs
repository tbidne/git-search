{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}

module Git.Search
  ( search,
  )
where

import Control.Category ((>>>))
import Control.Exception qualified as Ex
import Control.Exception.Utils qualified as Ex.Utils
import Control.Monad (when, (>=>))
import Data.Functor (void)
import Data.Text (Text)
import Data.Text qualified as T
import FileSystem.OsPath qualified as FS.OsP
import FileSystem.OsString (OsString, osstr)
import FileSystem.OsString qualified as FS.OsStr
#if MIN_VERSION_GLASGOW_HASKELL(9, 14, 1, 0)
import FileSystem.Path (Abs, Dir, Path, reldir, (<</>>), data MkPath)
#else
import FileSystem.Path (Abs, Dir, Path, reldir, (<</>>), pattern MkPath)
#endif
import FileSystem.Path qualified as FS.Path
import GHC.Stack.Types (HasCallStack)
import Git.Search.Args (Args (cache, debug, hash, repoName))
import System.Directory.OsPath qualified as Dir
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.Process qualified as P

data Env = MkEnv
  { args :: Args,
    root :: Path Abs Dir
  }

search :: Args -> IO ()
search args = Ex.bracket setup teardown $ \env -> do
  when args.debug $ do
    let rootStr = FS.OsP.decodeLenient $ FS.Path.toOsPath env.root
        repoStr = FS.OsP.decodeLenient $ FS.Path.toOsPath env.args.repoName
    putStrLn $ "Working directory: " <> rootStr
    putStrLn $ "Repo name: " <> repoStr

  cloneRepo env
  branches <- findBranches env

  case branches of
    [] -> putStrLn "No branches found."
    bs@(_ : _) -> do
      let formatted = mconcat $ fmap ("\n - " <>) bs
      putStrLn $
        mconcat
          [ "Found branches:",
            T.unpack formatted
          ]
  where
    setup = do
      tmpDir <- FS.Path.parseAbsDir =<< Dir.getTemporaryDirectory
      let root@(MkPath rootOsP) = tmpDir <</>> [reldir|git-search|]
      -- E.g. root := /tmp/git-search
      --
      -- Hence, repo := /tmp/git-search/org/some-repo
      --
      -- Create /tmp/git-search if it does not exist. The clone step will
      -- take care of creating the repo directory if necessary.
      Dir.createDirectoryIfMissing True rootOsP

      pure $
        MkEnv
          { root,
            args
          }

    teardown =
      if args.cache
        then \_ -> do
          putStrLn "Cache on, not removing cloned repo."
        else Dir.removePathForcibly . FS.Path.toOsPath . mkRepo

cloneRepo :: Env -> IO ()
cloneRepo env = do
  when env.args.debug $ do
    let repoStr = FS.OsP.decodeLenient repoOsP
    putStrLn $ "Clone destination: " <> repoStr

  doClone <-
    if env.args.cache
      then do
        exists <- Dir.doesDirectoryExist repoOsP

        when exists $ do
          putStrLn $ "Repo " <> repoNameStr <> " exists, skipping clone."

        pure (not exists)
      else pure True

  when doClone $ do
    putStrLn $ "Cloning " ++ repoNameStr ++ "..."
    void $ runGit gitArgs
    putStrLn "Clone finished."
  where
    -- osp NOT ospPathSep, as we do want the slash in repoName to be
    -- converted. Consequently, we want (<>) not (</>).
    --
    -- Also, default to https rather than ssh (git@github:) as the latter
    -- will fail without additional setup.
    src = [osstr|https://github.com/|] <> repoNameOsP

    -- Our args will make a bare repo with no files e.g. git clone org/some-repo
    -- in /tmp/git-search will create
    --
    --   /tmp/git-search/.git
    --
    -- But we still want our directory to be namespaced by repo name, hence
    -- we clone to repoOsP i.e. org/some-repo.
    gitArgs =
      [ [osstr|clone|],
        [osstr|--no-checkout|],
        [osstr|--filter=blob:none|],
        [osstr|--|],
        src,
        repoOsP
      ]

    repoOsP = FS.Path.toOsPath $ mkRepo env

    repoNameOsP = FS.Path.toOsPath env.args.repoName
    repoNameStr = FS.OsP.decodeLenient repoNameOsP

findBranches :: Env -> IO [Text]
findBranches env = do
  putStrLn $ "Searching for hash " ++ hashStr ++ "..."

  commitExists <- doesCommitExist env

  if not commitExists
    then do
      putStrLn "Commit does not exist."
      pure []
    else do
      Dir.withCurrentDirectory (FS.Path.toOsPath $ mkRepo env)
        $ runGitOut
          >=> FS.OsStr.decodeThrowM
          >>> fmap toText
        $ gitArgs
  where
    gitArgs =
      [ [osstr|branch|],
        [osstr|-r|],
        [osstr|--contains|],
        env.args.hash
      ]

    toText = fmap T.strip . T.lines . T.pack

    hashStr = FS.OsStr.decodeLenient env.args.hash

doesCommitExist :: Env -> IO Bool
doesCommitExist env = do
  (ec, _, _) <-
    Dir.withCurrentDirectory (FS.Path.toOsPath $ mkRepo env) $
      runGit gitArgs
  pure $ case ec of
    ExitSuccess -> True
    ExitFailure _ -> False
  where
    gitArgs =
      [ [osstr|cat-file|],
        [osstr|-e|],
        env.args.hash
      ]

runGitOut :: [OsString] -> IO OsString
runGitOut args = runProcessOut [osstr|git|] args

runGit :: [OsString] -> IO (ExitCode, String, String)
runGit args = runProcess [osstr|git|] args

runProcessOut :: (HasCallStack) => OsString -> [OsString] -> IO OsString
runProcessOut exe args = do
  let exeStr = FS.OsStr.decodeLenient exe
  (ec, out, err) <- runProcess exe args
  case ec of
    ExitFailure _ ->
      Ex.Utils.throwString $
        T.unpack $
          mconcat
            [ "Error running process '",
              T.pack exeStr,
              "' with args ",
              T.pack $ show args,
              ": ",
              T.pack err
            ]
    ExitSuccess -> FS.OsStr.encodeThrowM out

runProcess :: (HasCallStack) => OsString -> [OsString] -> IO (ExitCode, String, String)
runProcess exeStr argStrs = do
  exe <- FS.OsStr.decodeThrowM exeStr
  args <- traverse FS.OsStr.decodeThrowM argStrs
  P.readProcessWithExitCode exe args name
  where
    name = "runProcess"

mkRepo :: Env -> Path Abs Dir
mkRepo env = env.root <</>> env.args.repoName
