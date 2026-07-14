{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}

module Unit.Git.Search.Config (tests) where

import Effectful.FileSystem.PathReader.Dynamic qualified as PR
import Effectful.FileSystem.PathWriter.Dynamic qualified as PW
import Git.Search.Config qualified as Config
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Git.Search.Config"
    [ testGetRepoPathNoPath,
      testGetRepoPathWithPath
    ]

testGetRepoPathNoPath :: TestTree
testGetRepoPathNoPath = testCase desc $ do
  r <- runGetRepoPath name Nothing
  expected @=? r
  where
    desc = "Makes repo path without path"

    name = MkRepoName [osstr|org/repo|]

    expected = MkRepoPath $ root <</>> [reldirPathSep|.cache/git-search/org/repo|]

testGetRepoPathWithPath :: TestTree
testGetRepoPathWithPath = testCase desc $ do
  r <- runGetRepoPath name (Just path)
  expected @=? r
  where
    desc = "Makes repo path with path"

    name = MkRepoName [osstr|org/repo|]
    path = [ospPathSep|~/dev/my/repo2|]

    expected = MkRepoPath $ root <</>> [reldirPathSep|home/dev/my/repo2|]

runGetRepoPath :: RepoName -> Maybe OsPath -> IO RepoPath
runGetRepoPath name path = do
  runEff
    . runPR
    . runPW
    $ Config.getRepoPath name path

runPR :: Eff (PathReader : es) a -> Eff es a
runPR = interpret_ $ \case
  PR.GetHomeDirectory -> pure $ toOsPath root </> [osp|home|]
  PR.GetXdgDirectory PR.XdgConfig p -> pure $ toOsPath root </> [osp|.config|] </> p
  PR.GetXdgDirectory PR.XdgCache p -> pure $ toOsPath root </> [osp|.cache|] </> p
  other -> error $ "runPR: unimplemented: " ++ showEffectCons other

runPW :: Eff (PathWriter : es) a -> Eff es a
runPW = interpret_ $ \case
  PW.CreateDirectoryIfMissing _ _ -> pure ()
  other -> error $ "runPW: unimplemented: " ++ showEffectCons other
