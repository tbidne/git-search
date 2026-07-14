{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}

module Unit.Prelude
  ( module X,

    -- * Tests
    testProp1,

    -- * Paths
    root,
  )
where

import Git.Search.Config as X (Env (MkEnv, coreConfig))
import Git.Search.Config.Phase as X (ConfigPhase (ConfigPhaseEnv))
import Git.Search.Data as X
  ( Commit (MkCommit),
    RepoName (MkRepoName),
    RepoPath (MkRepoPath),
    RepoRemoteName (MkRepoRemoteName),
    RepoRemoteUri (MkRepoRemoteUri),
  )
import Git.Search.Prelude as X
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
import Test.Tasty as X (TestName, TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit as X (testCase, (@=?))
import Test.Tasty.Hedgehog as X (testPropertyNamed)

testProp1 :: TestName -> PropertyName -> PropertyT IO () -> TestTree
testProp1 testName propName =
  testPropertyNamed testName propName
    . withTests 1
    . property

root :: Path Abs Dir
#if WINDOWS
root = [absdir|C:\|]
#else
root = [absdir|/|]
#endif
