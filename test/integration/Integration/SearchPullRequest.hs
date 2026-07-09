{-# LANGUAGE QuasiQuotes #-}

module Integration.SearchPullRequest (tests) where

import Git.Search.Config.Data
  ( Command (SearchPullRequest),
    Config (MkConfig, clean, logColor, logLevel, repo),
    RepoConfig (MkRepoConfig, branches, domain, name, path, protocol, remoteName),
  )
import Git.Search.Logging.Data (LogLevel (LogLevelDebug))
import Integration.Prelude

tests :: TestTree
tests =
  testGroup
    "search-pr"
    [ testSearchPullRequestDefault,
      testSearchPullRequestArgs,
      testSearchPullRequestToml,
      testSearchPullRequestArgsOverridesToml,
      testSearchPullRequestNameReq,
      testSearchPullRequestDomainGithub,
      testSearchPullRequestProtocolHttps
    ]

testSearchPullRequestDefault :: TestTree
testSearchPullRequestDefault = testProp1 desc "testSearchPullRequestDefault" $ do
  (env, cmd) <- liftIO $ runEnvNoConfig args

  expectedEnv === env
  expectedCmd === cmd
  where
    desc = "Default config"

    args = ["--name", "org/repo", "search-pr", "123"]

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
      SearchPullRequest
        ( 123,
          MkRepoPath repoCacheDir,
          unsafeRemoteUri "https://github.com/org/repo",
          unsafeRepoName "org/repo"
        )

testSearchPullRequestArgs :: TestTree
testSearchPullRequestArgs = testProp1 desc "testSearchPullRequestArgs" $ do
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
        "--name",
        "org/repo",
        "--path",
        unsafeDecode $ toOsPath $ root <</>> [reldirPathSep|local/path/|],
        "--remote-name",
        "upstream",
        "search-pr",
        "123"
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
      SearchPullRequest
        ( 123,
          MkRepoPath $ root <</>> [reldirPathSep|local/path/|],
          unsafeRemoteUri "https://github.com/org/repo",
          unsafeRepoName "org/repo"
        )

testSearchPullRequestToml :: TestTree
testSearchPullRequestToml = testProp1 desc "testSearchPullRequestToml" $ do
  (env, cmd) <- liftIO $ runEnvConfig args

  expectedEnv === env
  expectedCmd === cmd
  where
    desc = "Toml config"

    args =
      [ -- have to override domain and protocol since they must be
        -- github and https.
        "--domain",
        "github.com",
        "--protocol",
        "https",
        "--name",
        "org/repo",
        "search-pr",
        "123"
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
      SearchPullRequest
        ( 123,
          MkRepoPath $ homeDir <</>> [reldirPathSep|local/path/|],
          unsafeRemoteUri "https://github.com/org/repo",
          unsafeRepoName "org/repo"
        )

testSearchPullRequestArgsOverridesToml :: TestTree
testSearchPullRequestArgsOverridesToml = testProp1 desc "testSearchPullRequestArgsOverridesToml" $ do
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
        "search-pr",
        "123"
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
      SearchPullRequest
        ( 123,
          MkRepoPath repoCacheDir,
          unsafeRemoteUri "https://github.com/org/repo",
          unsafeRepoName "org/repo"
        )

testSearchPullRequestNameReq :: TestTree
testSearchPullRequestNameReq = testProp1 desc "testSearchPullRequestNameReq" $ do
  eResult <- trySync $ liftIO $ runEnvNoConfig ["search-pr", "123"]

  case eResult of
    Left ex -> assertExStr expected ex
    Right x -> do
      annotate "Expected exception, received result"
      annotateShow x
      failure
  where
    desc = "Name is required"

    expected = "search-pr: Repository name must be specified by CLI args or Toml config."

testSearchPullRequestDomainGithub :: TestTree
testSearchPullRequestDomainGithub = testProp1 desc "testSearchPullRequestDomainGithub" $ do
  eResult <- trySync $ liftIO $ runEnvNoConfig args

  case eResult of
    Left ex -> assertExStr expected ex
    Right x -> do
      annotate "Expected exception, received result"
      annotateShow x
      failure
  where
    desc = "Domain must be github.com"

    args =
      [ "--domain",
        "server.com",
        "--name",
        "org/repo",
        "search-pr",
        "123"
      ]

    expected = "search-pr: --domain must be github.com, not: server.com"

testSearchPullRequestProtocolHttps :: TestTree
testSearchPullRequestProtocolHttps = testProp1 desc "testSearchPullRequestProtocolHttps" $ do
  eResult <- trySync $ liftIO $ runEnvNoConfig args

  case eResult of
    Left ex -> assertExStr expected ex
    Right x -> do
      annotate "Expected exception, received result"
      annotateShow x
      failure
  where
    desc = "Protocol must be https"

    args =
      [ "--protocol",
        "ssh",
        "--name",
        "org/repo",
        "search-pr",
        "123"
      ]

    expected = "search-pr: --protocol must be https, not: ssh"
