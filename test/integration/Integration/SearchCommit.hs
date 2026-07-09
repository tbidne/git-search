module Integration.SearchCommit (tests) where

import Git.Search.Config.Data
  ( Command (SearchCommit),
    Config (MkConfig, clean, logColor, logLevel, repo),
    RepoConfig (MkRepoConfig, branches, domain, name, path, protocol, remoteName),
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
              { clean = False,
                logColor = True,
                logLevel = Nothing,
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

    expectedCmd :: Command ConfigPhaseEnv
    expectedCmd =
      SearchCommit
        ( unsafeCommit "abcdef0",
          unsafeRepoPath "/.cache/git-search/org/repo/",
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
        "/local/path",
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
              { clean = True,
                logColor = False,
                logLevel = Just LogLevelDebug,
                repo =
                  MkRepoConfig
                    { branches = unsafeOsStrs ["*master", "*unstable*"],
                      domain = (),
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
        ( unsafeCommit "abcdef0",
          unsafeRepoPath "/local/path/",
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
              { clean = True,
                logColor = False,
                logLevel = Just LogLevelDebug,
                repo =
                  MkRepoConfig
                    { branches = unsafeOsStrs ["*master", "*unstable*"],
                      domain = (),
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
        ( unsafeCommit "abcdef0",
          unsafeRepoPath "/home/local/path/",
          unsafeRemoteUri "git@server.com:org/repo"
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
              { clean = False,
                logColor = True,
                logLevel = Nothing,
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

    expectedCmd :: Command ConfigPhaseEnv
    expectedCmd =
      SearchCommit
        ( unsafeCommit "abcdef0",
          unsafeRepoPath "/.cache/git-search/org/repo",
          unsafeRemoteUri "https://github.com/org/repo"
        )

testSearchCommitNameReq :: TestTree
testSearchCommitNameReq = testProp1 desc "testSearchCommitNameReq" $ do
  eResult <- trySync $ liftIO $ runEnvNoConfig ["search-commit", "abcdef0"]

  case eResult of
    Left ex -> expected === displayException ex
    Right x -> do
      annotate "Expected exception, received result"
      annotateShow x
      failure
  where
    desc = "Name is required"

    expected = "search-commit: Repository name must be specified by CLI args or Toml config."
