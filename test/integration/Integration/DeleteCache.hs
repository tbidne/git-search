{-# LANGUAGE QuasiQuotes #-}

module Integration.DeleteCache (tests) where

import Git.Search.Config.Data
  ( Command (DeleteCache),
    Config (MkConfig, auth, logColor, logLevel, repo, update),
    DeleteCacheType (DeleteCacheGlobal, DeleteCacheLocal),
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
  )
import Git.Search.Data (RepoUpdate (RepoUpdateAuto))
import Integration.Prelude

tests :: TestTree
tests =
  testGroup
    "delete-cache"
    [ testDeleteGlobal,
      testDeleteLocal
    ]

testDeleteGlobal :: TestTree
testDeleteGlobal = testProp1 desc "testDeleteGlobal" $ do
  (env, cmd) <- liftIO $ runEnvNoConfig ["delete-cache"]

  expectedEnv === env
  expectedCmd === cmd
  where
    desc = "Default global config"

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
    expectedCmd = DeleteCache $ DeleteCacheGlobal gitSearchCacheDir

testDeleteLocal :: TestTree
testDeleteLocal = testProp1 desc "testDeleteLocal" $ do
  (env, cmd) <- liftIO $ runEnvNoConfig ["--name", "org/repo", "delete-cache"]

  expectedEnv === env
  expectedCmd === cmd
  where
    desc = "Default local config"

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
      DeleteCache
        ( DeleteCacheLocal
            $ MkRepoPath
            $ gitSearchCacheDir
            <</>> [reldirPathSep|org/repo|]
        )
