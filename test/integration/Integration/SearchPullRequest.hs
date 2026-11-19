{-# LANGUAGE QuasiQuotes #-}

module Integration.SearchPullRequest (tests) where

import Git.Search.Config.Data
  ( Command (SearchPullRequest),
    Config (MkConfig, auth, logColor, logLevel, repo, update),
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
    SearchBaseParams
      ( MkSearchBaseParams,
        repoPath,
        repoSrc,
        shouldClone
      ),
    SearchPullRequestParams
      ( MkSearchPullRequestParams,
        baseParams,
        prNum,
        repoName
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
              { auth = Nothing,
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
                      remoteName = remoteNameDefault
                    },
                update = RepoUpdateAuto
              }
        }

    expectedCmd :: Command ConfigPhaseEnv
    expectedCmd =
      SearchPullRequest
        $ MkSearchPullRequestParams
          { baseParams =
              MkSearchBaseParams
                { repoPath = MkRepoPath repoCacheDir,
                  repoSrc = unsafeRemoteUri "https://github.com/org/repo",
                  shouldClone = True
                },
            prNum = 123,
            repoName = unsafeRepoName "org/repo"
          }

testSearchPullRequestArgs :: TestTree
testSearchPullRequestArgs = testProp1 desc "testSearchPullRequestArgs" $ do
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
              { auth = Nothing,
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
                      remoteName = unsafeRemoteName "upstream"
                    },
                update = RepoUpdateOff
              }
        }

    expectedCmd :: Command ConfigPhaseEnv
    expectedCmd =
      SearchPullRequest
        $ MkSearchPullRequestParams
          { baseParams =
              MkSearchBaseParams
                { repoPath = MkRepoPath $ root <</>> [reldirPathSep|local/path/|],
                  repoSrc = unsafeRemoteUri "https://github.com/org/repo",
                  shouldClone = True
                },
            prNum = 123,
            repoName = unsafeRepoName "org/repo"
          }

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
              { auth = Nothing,
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
                      remoteName = unsafeRemoteName "upstream"
                    },
                update = RepoUpdateClean
              }
        }

    expectedCmd :: Command ConfigPhaseEnv
    expectedCmd =
      SearchPullRequest
        $ MkSearchPullRequestParams
          { baseParams =
              MkSearchBaseParams
                { repoPath = MkRepoPath $ homeDir <</>> [reldirPathSep|local/path/|],
                  repoSrc = unsafeRemoteUri "https://github.com/org/repo",
                  shouldClone = True
                },
            prNum = 123,
            repoName = unsafeRepoName "org/repo"
          }

testSearchPullRequestArgsOverridesToml :: TestTree
testSearchPullRequestArgsOverridesToml = testProp1 desc "testSearchPullRequestArgsOverridesToml" $ do
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
        "search-pr",
        "123"
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
      SearchPullRequest
        $ MkSearchPullRequestParams
          { baseParams =
              MkSearchBaseParams
                { repoPath = MkRepoPath repoCacheDir,
                  repoSrc = unsafeRemoteUri "https://github.com/org/repo",
                  shouldClone = True
                },
            prNum = 123,
            repoName = unsafeRepoName "org/repo"
          }

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
