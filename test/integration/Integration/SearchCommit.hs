{-# LANGUAGE QuasiQuotes #-}

module Integration.SearchCommit (tests) where

import Git.Search.Config.Data
  ( Command (SearchCommit),
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
    SearchCommitParams
      ( MkSearchCommitParams,
        baseParams,
        commit
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

tests :: TestTree
tests =
  testGroup
    "search-commit"
    [ testSearchCommitDefault,
      testSearchCommitArgs,
      testSearchCommitToml,
      testSearchCommitTomlRepoDefaults,
      testSearchCommitTomlKey1,
      testSearchCommitTomlKey2,
      testSearchCommitTomlKey3,
      testSearchCommitTomlKeyMiss,
      testSearchCommitArgsOverridesToml,
      testSearchCommitNameReq
    ]

testSearchCommitDefault :: TestTree
testSearchCommitDefault = testProp1 desc "testSearchCommitDefault" $ do
  (env, cmd) <- liftIO $ runEnvNoConfig args

  expectedEnv === env
  expectedCmd === cmd
  where
    desc = "Default config"

    args = ["--name", "org/repo", "search-commit", "abcdef0"]

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
      SearchCommit
        $ MkSearchCommitParams
          { baseParams =
              MkSearchBaseParams
                { repoPath = MkRepoPath repoCacheDir,
                  repoSrc = unsafeRemoteUri "https://github.com/org/repo",
                  shouldClone = True
                },
            commit = unsafeCommit "abcdef0"
          }

testSearchCommitArgs :: TestTree
testSearchCommitArgs = testProp1 desc "testSearchCommitArgs" $ do
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
        "search-commit",
        "abcdef0"
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
      SearchCommit
        $ MkSearchCommitParams
          { baseParams =
              MkSearchBaseParams
                { repoPath = MkRepoPath $ root <</>> [reldirPathSep|local/path|],
                  repoSrc = unsafeRemoteUri "git@server.com:org/repo",
                  shouldClone = True
                },
            commit = unsafeCommit "abcdef0"
          }

testSearchCommitToml :: TestTree
testSearchCommitToml = testProp1 desc "testSearchCommitToml" $ do
  (env, cmd) <- liftIO $ runEnvConfig args

  expectedEnv === env
  expectedCmd === cmd
  where
    desc = "Toml config"

    args =
      [ "--name",
        "org/repo",
        "search-commit",
        "abcdef0"
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
      SearchCommit
        $ MkSearchCommitParams
          { baseParams =
              MkSearchBaseParams
                { repoPath = MkRepoPath $ homeDir <</>> [reldirPathSep|local/path/|],
                  repoSrc = unsafeRemoteUri "git@server.com:org/repo",
                  shouldClone = True
                },
            commit = unsafeCommit "abcdef0"
          }

testSearchCommitTomlRepoDefaults :: TestTree
testSearchCommitTomlRepoDefaults = testProp1 desc "testSearchCommitTomlRepoDefaults" $ do
  (env, cmd) <- liftIO $ runEnvConfig args

  expectedEnv === env
  expectedCmd === cmd
  where
    desc = "Toml config with repo defaults"

    args =
      [ "search-commit",
        "abcdef0"
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

    expectedCmd :: Command ConfigPhaseEnv
    expectedCmd =
      SearchCommit
        $ MkSearchCommitParams
          { baseParams =
              MkSearchBaseParams
                { repoPath = MkRepoPath $ root <</>> [reldirPathSep|.cache/git-search/my/repo/|],
                  repoSrc = unsafeRemoteUri "https://github.com/my/repo",
                  shouldClone = True
                },
            commit = unsafeCommit "abcdef0"
          }

testSearchCommitTomlKey1 :: TestTree
testSearchCommitTomlKey1 = testProp1 desc "testSearchCommitTomlKey1" $ do
  (env, cmd) <- liftIO $ runEnvConfig args

  expectedEnv === env
  expectedCmd === cmd
  where
    desc = "Toml config with key 1"

    args =
      [ "--name",
        "repo1",
        "search-commit",
        "abcdef0"
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
                    { branches = unsafeOsStrs ["*master1"],
                      branchesPrefix = (),
                      domain = (),
                      key = (),
                      name = (),
                      path = (),
                      protocol = (),
                      remoteName = unsafeRemoteName "upstream1"
                    },
                update = RepoUpdateClean
              }
        }

    expectedCmd :: Command ConfigPhaseEnv
    expectedCmd =
      SearchCommit
        $ MkSearchCommitParams
          { baseParams =
              MkSearchBaseParams
                { repoPath = MkRepoPath $ homeDir <</>> [reldirPathSep|local/path1/|],
                  repoSrc = unsafeRemoteUri "git@server1.com:org/repo_dupe",
                  shouldClone = True
                },
            commit = unsafeCommit "abcdef0"
          }

testSearchCommitTomlKey2 :: TestTree
testSearchCommitTomlKey2 = testProp1 desc "testSearchCommitTomlKey2" $ do
  (env, cmd) <- liftIO $ runEnvConfig args

  expectedEnv === env
  expectedCmd === cmd
  where
    desc = "Toml config with key 2"

    args =
      [ "--name",
        "repo2",
        "search-commit",
        "abcdef0"
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
                    { branches = unsafeOsStrs ["upstream2/master2", "upstream2/staging2*"],
                      branchesPrefix = (),
                      domain = (),
                      key = (),
                      name = (),
                      path = (),
                      protocol = (),
                      remoteName = unsafeRemoteName "upstream2"
                    },
                update = RepoUpdateClean
              }
        }

    expectedCmd :: Command ConfigPhaseEnv
    expectedCmd =
      SearchCommit
        $ MkSearchCommitParams
          { baseParams =
              MkSearchBaseParams
                { repoPath = MkRepoPath $ homeDir <</>> [reldirPathSep|local/path2/|],
                  repoSrc = unsafeRemoteUri "git@server2.com:org/repo_dupe",
                  shouldClone = True
                },
            commit = unsafeCommit "abcdef0"
          }

testSearchCommitTomlKey3 :: TestTree
testSearchCommitTomlKey3 = testProp1 desc "testSearchCommitTomlKey3" $ do
  (env, cmd) <- liftIO $ runEnvConfig args

  expectedEnv === env
  expectedCmd === cmd
  where
    desc = "Toml config with key 3"

    args =
      [ "--name",
        "repo3",
        "search-commit",
        "abcdef0"
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
                    { branches = unsafeOsStrs ["upstream3/master3"],
                      branchesPrefix = (),
                      domain = (),
                      key = (),
                      name = (),
                      path = (),
                      protocol = (),
                      remoteName = unsafeRemoteName "upstream3"
                    },
                update = RepoUpdateClean
              }
        }

    expectedCmd :: Command ConfigPhaseEnv
    expectedCmd =
      SearchCommit
        $ MkSearchCommitParams
          { baseParams =
              MkSearchBaseParams
                { repoPath = MkRepoPath $ root <</>> [reldirPathSep|.cache/git-search/repo3/|],
                  repoSrc = unsafeRemoteUri "git@server3.com:org/repo_dupe",
                  shouldClone = True
                },
            commit = unsafeCommit "abcdef0"
          }

testSearchCommitTomlKeyMiss :: TestTree
testSearchCommitTomlKeyMiss = testProp1 desc "testSearchCommitTomlKeyMiss" $ do
  (env, cmd) <- liftIO $ runEnvConfig args

  expectedEnv === env
  expectedCmd === cmd
  where
    desc = "Toml config with key miss"

    args =
      [ "--name",
        "repo_dupe",
        "search-commit",
        "abcdef0"
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

    expectedCmd :: Command ConfigPhaseEnv
    expectedCmd =
      SearchCommit
        $ MkSearchCommitParams
          { baseParams =
              MkSearchBaseParams
                { repoPath = MkRepoPath $ root <</>> [reldirPathSep|.cache/git-search/repo_dupe/|],
                  repoSrc = unsafeRemoteUri "https://github.com/repo_dupe",
                  shouldClone = True
                },
            commit = unsafeCommit "abcdef0"
          }

testSearchCommitArgsOverridesToml :: TestTree
testSearchCommitArgsOverridesToml = testProp1 desc "testSearchCommitArgsOverridesToml" $ do
  (env, cmd) <- liftIO $ runEnvConfig args

  expectedEnv === env
  expectedCmd === cmd
  where
    desc = "Args overrides toml config"

    args =
      [ "--update",
        "off",
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
        "search-commit",
        "abcdef0"
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
                update = RepoUpdateOff
              }
        }

    expectedCmd :: Command ConfigPhaseEnv
    expectedCmd =
      SearchCommit
        $ MkSearchCommitParams
          { baseParams =
              MkSearchBaseParams
                { repoPath = MkRepoPath repoCacheDir,
                  repoSrc = unsafeRemoteUri "https://github.com/org/repo",
                  shouldClone = True
                },
            commit = unsafeCommit "abcdef0"
          }

testSearchCommitNameReq :: TestTree
testSearchCommitNameReq = testProp1 desc "testSearchCommitNameReq" $ do
  eResult <- trySync $ liftIO $ runEnvNoConfig ["search-commit", "abcdef0"]

  case eResult of
    Left ex -> assertExStr expected ex
    Right x -> do
      annotate "Expected exception, received result"
      annotateShow x
      failure
  where
    desc = "Name is required"

    expected = "search-commit: Repository name must be specified by CLI args or Toml config."
