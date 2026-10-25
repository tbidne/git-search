{-# LANGUAGE QuasiQuotes #-}

module Integration.SearchDefaults (tests) where

import Git.Search.Config.Data
  ( Command (SearchCommit, SearchPullRequest),
    Config (MkConfig, auth, clean, logColor, logLevel, repo),
    RepoConfig (MkRepoConfig, branches, domain, name, path, protocol, remoteName),
    SearchCommitParams (MkSearchCommitParams),
    SearchPullRequestParams (MkSearchPullRequestParams),
  )
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

  ((env1, cmd1), (env2, cmd2)) <- case results of
    [x1, x2] -> pure (x1, x2)
    other -> do
      annotate "Expected exactly 2 elements"
      annotateShow other
      failure

  expectedEnv1 === env1
  expectedCmd1 === cmd1

  expectedEnv2 === env2
  expectedCmd2 === cmd2
  where
    desc = "Default config"

    args = ["search-defaults"]

    expectedEnv1 =
      MkEnv
        { coreConfig =
            MkConfig
              { auth = Nothing,
                clean = True,
                logColor = False,
                logLevel = Just LogLevelDebug,
                repo =
                  MkRepoConfig
                    { branches = [[osstr|*master|], [osstr|*unstable*|]],
                      domain = (),
                      name = (),
                      path = (),
                      protocol = (),
                      remoteName = Just (MkRepoRemoteName [osstr|upstream|])
                    }
              }
        }

    expectedCmd1 :: Command ConfigPhaseEnv
    expectedCmd1 =
      SearchCommit
        $ MkSearchCommitParams
          ( unsafeCommit "c190319",
            MkRepoPath $ root <</>> [reldirPathSep|home/local/path/|],
            unsafeRemoteUri "git@server.com:org/repo"
          )

    expectedEnv2 =
      MkEnv
        { coreConfig =
            MkConfig
              { auth = Nothing,
                clean = True,
                logColor = False,
                logLevel = Just LogLevelDebug,
                repo =
                  MkRepoConfig
                    { branches = [],
                      domain = (),
                      name = (),
                      path = (),
                      protocol = (),
                      remoteName = Nothing
                    }
              }
        }

    expectedCmd2 :: Command ConfigPhaseEnv
    expectedCmd2 =
      SearchPullRequest
        $ MkSearchPullRequestParams
          ( 510883,
            MkRepoPath $ root <</>> [reldirPathSep|.cache/git-search/org/repo2/|],
            unsafeRemoteUri "https://github.com/org/repo2",
            MkRepoName [osstr|org/repo2|]
          )
