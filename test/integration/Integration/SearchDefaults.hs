{-# LANGUAGE QuasiQuotes #-}

module Integration.SearchDefaults (tests) where

import Git.Search.Config.Data
  ( Command (SearchBranch, SearchCommit, SearchPullRequest),
    Config (MkConfig, auth, logColor, logLevel, repo, update),
    RepoConfig
      ( MkRepoConfig,
        branches,
        branchesPrefix,
        domain,
        key,
        name,
        path,
        protocol,
        remoteName
      ),
    SearchBaseParams
      ( MkSearchBaseParams,
        repoPath,
        repoSrc,
        shouldClone
      ),
    SearchBranchParams
      ( MkSearchBranchParams,
        baseParams,
        branch
      ),
    SearchCommitParams
      ( MkSearchCommitParams,
        baseParams,
        commit
      ),
    SearchPullRequestParams
      ( MkSearchPullRequestParams,
        baseParams,
        prNum,
        repoName
      ),
  )
import Git.Search.Data (RepoUpdate (RepoUpdateClean))
import Git.Search.Logging.Data (LogLevel (LogLevelDebug))
import Integration.Prelude

tests :: TestTree
tests =
  testGroup
    "search-defaults"
    [ testSearchDefaults
    ]

testSearchDefaults :: TestTree
testSearchDefaults = testProp1 desc "testSearchDefaults" $ do
  results <- liftIO $ runEnvConfigMany args

  ((env1, cmd1), (env2, cmd2), (env3, cmd3)) <- case results of
    [x1, x2, x3] -> pure (x1, x2, x3)
    other -> do
      annotate "Expected exactly 3 elements"
      annotateShow other
      failure

  expectedEnv1 === env1
  expectedCmd1 === cmd1

  expectedEnv2 === env2
  expectedCmd2 === cmd2

  expectedEnv3 === env3
  expectedCmd3 === cmd3
  where
    desc = "Default config"

    args = ["search-defaults"]

    expectedEnv1 =
      MkEnv
        { coreConfig =
            MkConfig
              { auth = Nothing,
                logColor = False,
                logLevel = Just LogLevelDebug,
                repo =
                  MkRepoConfig
                    { branches = [[osstr|*master|], [osstr|*unstable*|]],
                      branchesPrefix = (),
                      domain = (),
                      key = (),
                      name = (),
                      path = (),
                      protocol = (),
                      remoteName = MkRepoRemoteName [osstr|upstream|]
                    },
                update = RepoUpdateClean
              }
        }

    expectedCmd1 :: Command ConfigPhaseEnv
    expectedCmd1 =
      SearchCommit
        $ MkSearchCommitParams
          { baseParams =
              MkSearchBaseParams
                { repoPath = MkRepoPath $ root <</>> [reldirPathSep|home/local/path/|],
                  repoSrc = unsafeRemoteUri "git@server.com:org/repo",
                  shouldClone = True
                },
            commit = unsafeCommit "c190319"
          }

    expectedEnv2 =
      MkEnv
        { coreConfig =
            MkConfig
              { auth = Nothing,
                logColor = False,
                logLevel = Just LogLevelDebug,
                repo =
                  MkRepoConfig
                    { branches = [],
                      branchesPrefix = (),
                      domain = (),
                      key = (),
                      name = (),
                      path = (),
                      protocol = (),
                      remoteName = remoteNameDefault
                    },
                update = RepoUpdateClean
              }
        }

    expectedCmd2 :: Command ConfigPhaseEnv
    expectedCmd2 =
      SearchBranch
        $ MkSearchBranchParams
          { baseParams =
              MkSearchBaseParams
                { repoPath = MkRepoPath $ root <</>> [reldirPathSep|.cache/git-search/org/repo2/|],
                  repoSrc = unsafeRemoteUri "https://github.com/org/repo2",
                  shouldClone = True
                },
            branch = MkBranch [osstr|master|]
          }

    expectedEnv3 = expectedEnv2

    expectedCmd3 :: Command ConfigPhaseEnv
    expectedCmd3 =
      SearchPullRequest
        $ MkSearchPullRequestParams
          { baseParams =
              MkSearchBaseParams
                { repoPath = MkRepoPath $ root <</>> [reldirPathSep|.cache/git-search/org/repo2/|],
                  repoSrc = unsafeRemoteUri "https://github.com/org/repo2",
                  shouldClone = True
                },
            prNum = 510883,
            repoName = MkRepoName [osstr|org/repo2|]
          }
