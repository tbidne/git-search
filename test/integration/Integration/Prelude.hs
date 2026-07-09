{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}

module Integration.Prelude
  ( module X,

    -- * Testers
    testProp1,

    -- * Env runners
    runEnv,
    runEnvNoConfig,
    runEnvConfig,

    -- * Assertions
    assertExStr,

    -- * Misc

    -- ** Directories
    xdgCacheDir,
    gitSearchCacheDir,
    repoCacheDir,
    xdgConfigDir,
    homeDir,
    root,

    -- ** Basic types
    unsafeOsStrs,

    -- ** Core types
    unsafeCommit,
    unsafePathAbsDir,
    unsafeRemoteName,
    unsafeRemoteUri,
    unsafeRepoName,
    unsafeRepoPath,
  )
where

import Data.Text qualified as T
import Effectful.Dispatch.Dynamic (interpret_)
import Effectful.FileSystem.PathReader.Dynamic qualified as PR
import Effectful.FileSystem.PathWriter.Dynamic qualified as PW
import FileSystem.Path qualified as FS.Path
import Git.Search.Config as X (Env (MkEnv, coreConfig))
import Git.Search.Config.Data (Command)
import Git.Search.Config.Phase as X (ConfigPhase (ConfigPhaseEnv))
import Git.Search.Data as X
  ( Commit (MkCommit),
    RepoName (MkRepoName),
    RepoPath (MkRepoPath),
    RepoRemoteName (MkRepoRemoteName),
    RepoRemoteUri (MkRepoRemoteUri),
  )
import Git.Search.Prelude as X
import Git.Search.Runner qualified as Runner
import Hedgehog as X
  ( PropertyName,
    PropertyT,
    annotate,
    annotateShow,
    assert,
    failure,
    property,
    withTests,
    (===),
  )
import System.Environment qualified as Env
import Test.Tasty as X (TestName, TestTree, defaultMain, testGroup)
import Test.Tasty.Hedgehog as X (testPropertyNamed)

testProp1 :: TestName -> PropertyName -> PropertyT IO () -> TestTree
testProp1 testName propName =
  testPropertyNamed testName propName
    . withTests 1
    . property

runEnvNoConfig :: [String] -> IO (Env, Command ConfigPhaseEnv)
runEnvNoConfig = runEnv . (\as -> "--config" : "off" : as)

runEnvConfig :: [String] -> IO (Env, Command ConfigPhaseEnv)
runEnvConfig = runEnv . (\as -> "--config" : cfg : as)
  where
    cfg = unsafeDecode [ospPathSep|test/integration/config.toml|]

-- NOTE: withArgs is not thread-safe, hence our cabal suite cannot have:
--
--   ghc-options:    -threaded -with-rtsopts=-N
--
-- Alternatively, we could use tasty to run sequentially.
runEnv :: [String] -> IO (Env, Command ConfigPhaseEnv)
runEnv args = Env.withArgs args $ runner Runner.getEnv
  where
    runner =
      runEff
        . runFileReader
        . runOptparse
        . runPR
        . runPW

runPR :: Eff (PathReader : es) a -> Eff es a
runPR = interpret_ $ \case
  PR.GetXdgDirectory xdg p -> case xdg of
    PR.XdgCache -> pure $ toOsPath xdgCacheDir </> p
    PR.XdgConfig -> pure $ toOsPath xdgConfigDir </> p
    other -> error $ "runPR.xdg.unimplemented: " ++ show other
  PR.GetHomeDirectory -> pure $ toOsPath homeDir
  other -> error $ "runPR.unimplemented: " ++ showEffectCons other

xdgCacheDir :: Path Abs Dir
xdgCacheDir = root <</>> [reldir|.cache|]

gitSearchCacheDir :: Path Abs Dir
gitSearchCacheDir = xdgCacheDir <</>> [reldir|git-search|]

repoCacheDir :: Path Abs Dir
repoCacheDir = gitSearchCacheDir <</>> [reldirPathSep|org/repo|]

xdgConfigDir :: Path Abs Dir
xdgConfigDir = root <</>> [reldir|.config|]

homeDir :: Path Abs Dir
homeDir = root <</>> [reldir|home|]

runPW :: Eff (PathWriter : es) a -> Eff es a
runPW = interpret_ $ \case
  PW.CreateDirectoryIfMissing _ _ -> pure ()
  other -> error $ "runPW.unimplemented: " ++ showEffectCons other

unsafeOsStrs :: [String] -> [OsString]
unsafeOsStrs = fmap unsafeEncode

unsafeCommit :: String -> Commit
unsafeCommit = MkCommit . unsafeEncode

unsafePathAbsDir :: String -> Path Abs Dir
unsafePathAbsDir s = case k s of
  Nothing -> error $ "unsafePathAbsDir: " ++ s
  Just p -> p
  where
    k = encodeValidThrowM >=> FS.Path.parseAbsDir

unsafeRemoteName :: String -> RepoRemoteName
unsafeRemoteName = MkRepoRemoteName . unsafeEncode

unsafeRemoteUri :: String -> RepoRemoteUri
unsafeRemoteUri = MkRepoRemoteUri . unsafeEncode

unsafeRepoName :: String -> RepoName
unsafeRepoName = MkRepoName . unsafeEncode

unsafeRepoPath :: String -> RepoPath
unsafeRepoPath = MkRepoPath . unsafePathAbsDir

assertExStr :: (Exception e) => String -> e -> PropertyT IO ()
assertExStr expected ex = do
  annotate expected
  annotate result

  -- isPrefixOf for GHC 9.10 and trailing exception callstack.
  assert (expectedTxt `T.isPrefixOf` resultTxt)
  where
    expectedTxt = pack expected
    result = displayException ex
    resultTxt = pack $ displayException ex

root :: Path Abs Dir
#if WINDOWS
root = [absdir|C:\|]
#else
root = [absdir|/|]
#endif
