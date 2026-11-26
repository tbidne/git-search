{-# LANGUAGE QuasiQuotes #-}

module Integration.SearchBranch (tests) where

import Git.Search.Config.Data
  ( Command (SearchBranch),
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
  )
import Git.Search.Data
  ( RepoUpdate
      ( RepoUpdateAuto,
        RepoUpdateClean,
        RepoUpdateOff
      ),
  )
import Git.Search.Logging.Data (LogLevel (LogLevelDebug))
import Integration.Prelude

-- NOTE:
--
-- Essentially a copy of search-commit.

tests :: TestTree
tests =
  testGroup
    "search-branch"
    [ testSearchBranchDefault,
      testSearchBranchArgs,
      testSearchBranchToml,
      testSearchBranchArgsOverridesToml,
      testSearchBranchNameReq
    ]

testSearchBranchDefault :: TestTree
testSearchBranchDefault = testProp1 desc "testSearchBranchDefault" $ do
  (env, cmd) <- liftIO $ runEnvNoConfig args

  expectedEnv === env
  expectedCmd === cmd
  where
    desc = "Default config"

    args = ["--name", "org/repo", "search-branch", "main"]

    expectedEnv =
      MkEnv
        { coreConfig =
            MkConfig
              { auth = Nothing,
                logColor = True,
                logLevel = Nothing,
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
                update = RepoUpdateAuto
              }
        }

    expectedCmd :: Command ConfigPhaseEnv
    expectedCmd =
      SearchBranch
        $ MkSearchBranchParams
          { baseParams =
              MkSearchBaseParams
                { repoPath = MkRepoPath repoCacheDir,
                  repoSrc = unsafeRemoteUri "https://github.com/org/repo",
                  shouldClone = True
                },
            branch = unsafeBranch "main"
          }

testSearchBranchArgs :: TestTree
testSearchBranchArgs = testProp1 desc "testSearchBranchArgs" $ do
  (env, cmd) <- liftIO $ runEnvNoConfig args

  expectedEnv === env
  expectedCmd === cmd
  where
    desc = "CLI args config"

    args =
      [ "--update",
        "off",
        "--log-color",
        "off",
        "--log-level",
        "debug",
        "--branches",
        "*master *unstable*",
        "--branches-prefix",
        "off",
        "--domain",
        "server.com",
        "--name",
        "org/repo",
        "--path",
        unsafeDecode $ toOsPath $ root <</>> [reldirPathSep|local/path/|],
        "--protocol",
        "ssh",
        "--remote-name",
        "upstream",
        "search-branch",
        "main"
      ]

    expectedEnv =
      MkEnv
        { coreConfig =
            MkConfig
              { auth = Nothing,
                logColor = False,
                logLevel = Just LogLevelDebug,
                repo =
                  MkRepoConfig
                    { branches = unsafeOsStrs ["*master", "*unstable*"],
                      branchesPrefix = (),
                      domain = (),
                      key = (),
                      name = (),
                      path = (),
                      protocol = (),
                      remoteName = unsafeRemoteName "upstream"
                    },
                update = RepoUpdateOff
              }
        }

    expectedCmd :: Command ConfigPhaseEnv
    expectedCmd =
      SearchBranch
        $ MkSearchBranchParams
          { baseParams =
              MkSearchBaseParams
                { repoPath = MkRepoPath $ root <</>> [reldirPathSep|local/path|],
                  repoSrc = unsafeRemoteUri "git@server.com:org/repo",
                  shouldClone = True
                },
            branch = unsafeBranch "main"
          }

testSearchBranchToml :: TestTree
testSearchBranchToml = testProp1 desc "testSearchBranchToml" $ do
  (env, cmd) <- liftIO $ runEnvConfig args

  expectedEnv === env
  expectedCmd === cmd
  where
    desc = "Toml config"

    args =
      [ "--name",
        "org/repo",
        "search-branch",
        "main"
      ]

    expectedEnv =
      MkEnv
        { coreConfig =
            MkConfig
              { auth = Nothing,
                logColor = False,
                logLevel = Just LogLevelDebug,
                repo =
                  MkRepoConfig
                    { branches = unsafeOsStrs ["*master", "*unstable*"],
                      branchesPrefix = (),
                      domain = (),
                      key = (),
                      name = (),
                      path = (),
                      protocol = (),
                      remoteName = unsafeRemoteName "upstream"
                    },
                update = RepoUpdateClean
              }
        }

    expectedCmd :: Command ConfigPhaseEnv
    expectedCmd =
      SearchBranch
        $ MkSearchBranchParams
          { baseParams =
              MkSearchBaseParams
                { repoPath = MkRepoPath $ homeDir <</>> [reldirPathSep|local/path/|],
                  repoSrc = unsafeRemoteUri "git@server.com:org/repo",
                  shouldClone = True
                },
            branch = unsafeBranch "main"
          }

testSearchBranchArgsOverridesToml :: TestTree
testSearchBranchArgsOverridesToml = testProp1 desc "testSearchBranchArgsOverridesToml" $ do
  (env, cmd) <- liftIO $ runEnvConfig args

  expectedEnv === env
  expectedCmd === cmd
  where
    desc = "Args overrides toml config"

    args =
      [ "--update",
        "auto",
        "--log-color",
        "on",
        "--log-level",
        "off",
        "--branches",
        "off",
        "--domain",
        "off",
        "--name",
        "org/repo",
        "--path",
        "off",
        "--protocol",
        "off",
        "--remote-name",
        "off",
        "search-branch",
        "main"
      ]

    expectedEnv =
      MkEnv
        { coreConfig =
            MkConfig
              { auth = Nothing,
                logColor = True,
                logLevel = Nothing,
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
                update = RepoUpdateAuto
              }
        }

    expectedCmd :: Command ConfigPhaseEnv
    expectedCmd =
      SearchBranch
        $ MkSearchBranchParams
          { baseParams =
              MkSearchBaseParams
                { repoPath = MkRepoPath repoCacheDir,
                  repoSrc = unsafeRemoteUri "https://github.com/org/repo",
                  shouldClone = True
                },
            branch = unsafeBranch "main"
          }

testSearchBranchNameReq :: TestTree
testSearchBranchNameReq = testProp1 desc "testSearchBranchNameReq" $ do
  eResult <- trySync $ liftIO $ runEnvNoConfig ["search-branch", "main"]

  case eResult of
    Left ex -> assertExStr expected ex
    Right x -> do
      annotate "Expected exception, received result"
      annotateShow x
      failure
  where
    desc = "Name is required"

    expected = "search-branch: Repository name must be specified by CLI args or Toml config."
