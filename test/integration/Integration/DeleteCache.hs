{-# LANGUAGE QuasiQuotes #-}

module Integration.DeleteCache (tests) where

import Git.Search.Config.Data
  ( Command (DeleteCache),
    Config (MkConfig, clean, logColor, logLevel, repo),
    DeleteCacheType (DeleteCacheGlobal, DeleteCacheLocal),
    RepoConfig (MkRepoConfig, branches, domain, name, path, protocol, remoteName),
  )
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
      DeleteCache
        ( DeleteCacheLocal
            $ MkRepoPath
            $ gitSearchCacheDir
            <</>> [reldirPathSep|org/repo|]
        )
