{-# LANGUAGE QuasiQuotes #-}

module Integration.SearchCommit (tests) where

import Git.Search.Config.Data
  ( Command (SearchCommit),
    Config (MkConfig, auth, clean, logColor, logLevel, repo),
    RepoConfig
      ( MkRepoConfig,
        branches,
        domain,
        key,
        name,
        path,
        protocol,
        remoteName
      ),
    SearchCommitParams (MkSearchCommitParams),
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
                clean = False,
                logColor = True,
                logLevel = Nothing,
                repo =
                  MkRepoConfig
                    { branches = [],
                      domain = (),
                      key = (),
                      name = (),
                      path = (),
                      protocol = (),
                      remoteName = Nothing
                    }
              }
        }

    expectedCmd :: Command ConfigPhaseEnv
    expectedCmd =
      SearchCommit
        $ MkSearchCommitParams
          ( unsafeCommit "abcdef0",
            MkRepoPath repoCacheDir,
            unsafeRemoteUri "https://github.com/org/repo"
          )

testSearchCommitArgs :: TestTree
testSearchCommitArgs = testProp1 desc "testSearchCommitArgs" $ do
  (env, cmd) <- liftIO $ runEnvNoConfig args

  expectedEnv === env
  expectedCmd === cmd
  where
    desc = "CLI args config"

    args =
      [ "--clean",
        "on",
        "--log-color",
        "off",
        "--log-level",
        "debug",
        "--branches",
        "*master *unstable*",
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
                clean = True,
                logColor = False,
                logLevel = Just LogLevelDebug,
                repo =
                  MkRepoConfig
                    { branches = unsafeOsStrs ["*master", "*unstable*"],
                      domain = (),
                      key = (),
                      name = (),
                      path = (),
                      protocol = (),
                      remoteName = Just $ unsafeRemoteName "upstream"
                    }
              }
        }

    expectedCmd :: Command ConfigPhaseEnv
    expectedCmd =
      SearchCommit
        $ MkSearchCommitParams
          ( unsafeCommit "abcdef0",
            MkRepoPath $ root <</>> [reldirPathSep|local/path|],
            unsafeRemoteUri "git@server.com:org/repo"
          )

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
                clean = True,
                logColor = False,
                logLevel = Just LogLevelDebug,
                repo =
                  MkRepoConfig
                    { branches = unsafeOsStrs ["*master", "*unstable*"],
                      domain = (),
                      key = (),
                      name = (),
                      path = (),
                      protocol = (),
                      remoteName = Just $ unsafeRemoteName "upstream"
                    }
              }
        }

    expectedCmd :: Command ConfigPhaseEnv
    expectedCmd =
      SearchCommit
        $ MkSearchCommitParams
          ( unsafeCommit "abcdef0",
            MkRepoPath $ homeDir <</>> [reldirPathSep|local/path/|],
            unsafeRemoteUri "git@server.com:org/repo"
          )

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
                clean = True,
                logColor = False,
                logLevel = Just LogLevelDebug,
                repo =
                  MkRepoConfig
                    { branches = [],
                      domain = (),
                      key = (),
                      name = (),
                      path = (),
                      protocol = (),
                      remoteName = Nothing
                    }
              }
        }

    expectedCmd :: Command ConfigPhaseEnv
    expectedCmd =
      SearchCommit
        $ MkSearchCommitParams
          ( unsafeCommit "abcdef0",
            MkRepoPath $ root <</>> [reldirPathSep|.cache/git-search/my/repo/|],
            unsafeRemoteUri "https://github.com/my/repo"
          )

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
                clean = True,
                logColor = False,
                logLevel = Just LogLevelDebug,
                repo =
                  MkRepoConfig
                    { branches = unsafeOsStrs ["*master1"],
                      domain = (),
                      key = (),
                      name = (),
                      path = (),
                      protocol = (),
                      remoteName = Just $ unsafeRemoteName "upstream1"
                    }
              }
        }

    expectedCmd :: Command ConfigPhaseEnv
    expectedCmd =
      SearchCommit
        $ MkSearchCommitParams
          ( unsafeCommit "abcdef0",
            MkRepoPath $ homeDir <</>> [reldirPathSep|local/path1/|],
            unsafeRemoteUri "git@server1.com:org/repo_dupe"
          )

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
                clean = True,
                logColor = False,
                logLevel = Just LogLevelDebug,
                repo =
                  MkRepoConfig
                    { branches = unsafeOsStrs ["*master2"],
                      domain = (),
                      key = (),
                      name = (),
                      path = (),
                      protocol = (),
                      remoteName = Just $ unsafeRemoteName "upstream2"
                    }
              }
        }

    expectedCmd :: Command ConfigPhaseEnv
    expectedCmd =
      SearchCommit
        $ MkSearchCommitParams
          ( unsafeCommit "abcdef0",
            MkRepoPath $ homeDir <</>> [reldirPathSep|local/path2/|],
            unsafeRemoteUri "git@server2.com:org/repo_dupe"
          )

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
                clean = True,
                logColor = False,
                logLevel = Just LogLevelDebug,
                repo =
                  MkRepoConfig
                    { branches = [],
                      domain = (),
                      key = (),
                      name = (),
                      path = (),
                      protocol = (),
                      remoteName = Nothing
                    }
              }
        }

    expectedCmd :: Command ConfigPhaseEnv
    expectedCmd =
      SearchCommit
        $ MkSearchCommitParams
          ( unsafeCommit "abcdef0",
            MkRepoPath $ root <</>> [reldirPathSep|.cache/git-search/repo_dupe/|],
            unsafeRemoteUri "https://github.com/repo_dupe"
          )

testSearchCommitArgsOverridesToml :: TestTree
testSearchCommitArgsOverridesToml = testProp1 desc "testSearchCommitArgsOverridesToml" $ do
  (env, cmd) <- liftIO $ runEnvConfig args

  expectedEnv === env
  expectedCmd === cmd
  where
    desc = "Args overrides toml config"

    args =
      [ "--clean",
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
                clean = False,
                logColor = True,
                logLevel = Nothing,
                repo =
                  MkRepoConfig
                    { branches = [],
                      domain = (),
                      key = (),
                      name = (),
                      path = (),
                      protocol = (),
                      remoteName = Nothing
                    }
              }
        }

    expectedCmd :: Command ConfigPhaseEnv
    expectedCmd =
      SearchCommit
        $ MkSearchCommitParams
          ( unsafeCommit "abcdef0",
            MkRepoPath repoCacheDir,
            unsafeRemoteUri "https://github.com/org/repo"
          )

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
