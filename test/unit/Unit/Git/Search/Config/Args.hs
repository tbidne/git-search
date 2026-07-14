{-# LANGUAGE QuasiQuotes #-}

module Unit.Git.Search.Config.Args (tests) where

import Git.Search.Config.Args (Args (MkArgs))
import Git.Search.Config.Args qualified as Args
import Git.Search.Config.Data
  ( Command (DeleteCache, SearchCommit, SearchPullRequest),
    Config (MkConfig, auth, clean, logColor, logLevel, repo),
    RepoConfig
      ( MkRepoConfig,
        branches,
        domain,
        name,
        path,
        protocol,
        remoteName
      ),
  )
import Git.Search.Config.WithDisabled
  ( WithDisabled (Disabled, With),
  )
import Git.Search.Data
  ( Domain (MkDomain),
    Protocol (ProtocolHttps, ProtocolSsh),
  )
import Git.Search.Logging.Data
  ( LogLevel (LogLevelDebug, LogLevelInfo),
  )
import System.Environment qualified as Env
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Git.Search.Config.Args"
    [ commandTests,
      testAuth,
      loggingTests,
      repoTests,
      miscTests
    ]

commandTests :: TestTree
commandTests =
  testGroup
    "Commands"
    [ testDeleteCache,
      testSearchCommit,
      testSearchPullRequest
    ]

testDeleteCache :: TestTree
testDeleteCache = testProp1 desc "testDeleteCache" $ do
  testGetArgs e args
  where
    desc = "Parses default delete"

    args = ["delete-cache"]

    e = set' #command (DeleteCache ()) defArgs

testSearchCommit :: TestTree
testSearchCommit = testProp1 desc "testSearchCommit" $ do
  testGetArgs defArgs args
  where
    desc = "Parses default search-commit"

    args =
      [ "search-commit",
        commitStr
      ]

testSearchPullRequest :: TestTree
testSearchPullRequest = testProp1 desc "testSearchPullRequest" $ do
  testGetArgs e args
  where
    desc = "Parses default search-pr"

    args =
      [ "search-pr",
        "123"
      ]

    e = set' #command (SearchPullRequest 123) defArgs

testAuth :: TestTree
testAuth = testProp1 desc "testAuth" $ do
  testGetArgs e args
  where
    desc = "Parses --auth"

    args =
      [ "--auth",
        "<token>",
        "search-commit",
        commitStr
      ]

    e = set' (#coreConfig % #auth) (Just [osstr|<token>|]) defArgs

loggingTests :: TestTree
loggingTests =
  testGroup
    "Logging options"
    [ testLogColorOn,
      testLogColorOff,
      testLogLevelDebug,
      testLogLevelInfo,
      testLogLevelOff
    ]

testLogColorOn :: TestTree
testLogColorOn = testProp1 desc "testLogColorOn" $ do
  testGetArgs e args
  where
    desc = "Parses --log-color on"

    args =
      [ "--log-color",
        "on",
        "search-commit",
        commitStr
      ]

    e = set' (#coreConfig % #logColor) (Just True) defArgs

testLogColorOff :: TestTree
testLogColorOff = testProp1 desc "testLogColorOff" $ do
  testGetArgs e args
  where
    desc = "Parses --log-color off"

    args =
      [ "--log-color",
        "off",
        "search-commit",
        commitStr
      ]

    e = set' (#coreConfig % #logColor) (Just False) defArgs

testLogLevelDebug :: TestTree
testLogLevelDebug = testProp1 desc "testLogLevelDebug" $ do
  testGetArgs e args
  where
    desc = "Parses --log-level debug"

    args =
      [ "--log-level",
        "debug",
        "search-commit",
        commitStr
      ]

    e = set' (#coreConfig % #logLevel) (Just $ With LogLevelDebug) defArgs

testLogLevelInfo :: TestTree
testLogLevelInfo = testProp1 desc "testLogLevelInfo" $ do
  testGetArgs e args
  where
    desc = "Parses --log-level info"

    args =
      [ "--log-level",
        "info",
        "search-commit",
        commitStr
      ]

    e = set' (#coreConfig % #logLevel) (Just $ With LogLevelInfo) defArgs

testLogLevelOff :: TestTree
testLogLevelOff = testProp1 desc "testLogLevelOff" $ do
  testGetArgs e args
  where
    desc = "Parses --log-level off"

    args =
      [ "--log-level",
        "off",
        "search-commit",
        commitStr
      ]

    e = set' (#coreConfig % #logLevel) (Just Disabled) defArgs

repoTests :: TestTree
repoTests =
  testGroup
    "Repository options"
    [ testBranchesOn,
      testBranchesOff,
      testDomain,
      testDomainOff,
      testName,
      testNameOff,
      testPath,
      testPathOff,
      testProtocolHttps,
      testProtocolSsh,
      testProtocolOff,
      testRemoteName,
      testRemoteNameOff
    ]

testBranchesOn :: TestTree
testBranchesOn = testProp1 desc "testBranchesOn" $ do
  testGetArgs e args
  where
    desc = "Parses --branches"

    args =
      [ "--branches",
        "*b1 *b2* foo",
        "search-commit",
        commitStr
      ]

    bs =
      [ [osstr|*b1|],
        [osstr|*b2*|],
        [osstr|foo|]
      ]

    e = set' (#coreConfig % #repo % #branches) (Just $ With bs) defArgs

testBranchesOff :: TestTree
testBranchesOff = testProp1 desc "testBranchesOff" $ do
  testGetArgs e args
  where
    desc = "Parses --branches off"

    args =
      [ "--branches",
        "off",
        "search-commit",
        commitStr
      ]

    e = set' (#coreConfig % #repo % #branches) (Just Disabled) defArgs

testDomain :: TestTree
testDomain = testProp1 desc "testDomain" $ do
  testGetArgs e args
  where
    desc = "Parses --domain"

    args =
      [ "--domain",
        "some-domain",
        "search-commit",
        commitStr
      ]

    e =
      set'
        (#coreConfig % #repo % #domain)
        (Just $ With $ MkDomain [osstr|some-domain|])
        defArgs

testDomainOff :: TestTree
testDomainOff = testProp1 desc "testDomainOff" $ do
  testGetArgs e args
  where
    desc = "Parses --domain off"

    args =
      [ "--domain",
        "off",
        "search-commit",
        commitStr
      ]

    e = set' (#coreConfig % #repo % #domain) (Just Disabled) defArgs

testName :: TestTree
testName = testProp1 desc "testName" $ do
  testGetArgs e args
  where
    desc = "Parses --name"

    args =
      [ "--name",
        "org/repo",
        "search-commit",
        commitStr
      ]

    e =
      set'
        (#coreConfig % #repo % #name)
        ( Just
            $ With
            $ MkRepoName [osstr|org/repo|]
        )
        defArgs

testNameOff :: TestTree
testNameOff = testProp1 desc "testNameOff" $ do
  testGetArgs e args
  where
    desc = "Parses --name off"

    args =
      [ "--name",
        "off",
        "search-commit",
        commitStr
      ]

    e = set' (#coreConfig % #repo % #name) (Just Disabled) defArgs

testPath :: TestTree
testPath = testProp1 desc "testPath" $ do
  testGetArgs e args
  where
    desc = "Parses --path"

    args =
      [ "--path",
        unsafeDecode path,
        "search-commit",
        commitStr
      ]

    path = [ospPathSep|~/dev/repo2|]

    e =
      set'
        (#coreConfig % #repo % #path)
        ( Just
            $ With
              path
        )
        defArgs

testPathOff :: TestTree
testPathOff = testProp1 desc "testPathOff" $ do
  testGetArgs e args
  where
    desc = "Parses --path off"

    args =
      [ "--path",
        "off",
        "search-commit",
        commitStr
      ]

    e = set' (#coreConfig % #repo % #path) (Just Disabled) defArgs

testProtocolHttps :: TestTree
testProtocolHttps = testProp1 desc "testProtocolHttps" $ do
  testGetArgs e args
  where
    desc = "Parses --path https"

    args =
      [ "--protocol",
        "https",
        "search-commit",
        commitStr
      ]

    e =
      set'
        (#coreConfig % #repo % #protocol)
        ( Just
            $ With
              ProtocolHttps
        )
        defArgs

testProtocolSsh :: TestTree
testProtocolSsh = testProp1 desc "testProtocolSsh" $ do
  testGetArgs e args
  where
    desc = "Parses --path ssh"

    args =
      [ "--protocol",
        "ssh",
        "search-commit",
        commitStr
      ]

    e =
      set'
        (#coreConfig % #repo % #protocol)
        ( Just
            $ With
              ProtocolSsh
        )
        defArgs

testProtocolOff :: TestTree
testProtocolOff = testProp1 desc "testProtocolOff" $ do
  testGetArgs e args
  where
    desc = "Parses --protocol off"

    args =
      [ "--protocol",
        "off",
        "search-commit",
        commitStr
      ]

    e = set' (#coreConfig % #repo % #protocol) (Just Disabled) defArgs

testRemoteName :: TestTree
testRemoteName = testProp1 desc "testRemoteName" $ do
  testGetArgs e args
  where
    desc = "Parses --remote-name"

    args =
      [ "--remote-name",
        "upstream",
        "search-commit",
        commitStr
      ]

    e =
      set'
        (#coreConfig % #repo % #remoteName)
        ( Just
            $ With
            $ MkRepoRemoteName [osstr|upstream|]
        )
        defArgs

testRemoteNameOff :: TestTree
testRemoteNameOff = testProp1 desc "testRemoteNameOff" $ do
  testGetArgs e args
  where
    desc = "Parses --remote-name"

    args =
      [ "--remote-name",
        "off",
        "search-commit",
        commitStr
      ]

    e =
      set'
        (#coreConfig % #repo % #remoteName)
        (Just Disabled)
        defArgs

miscTests :: TestTree
miscTests =
  testGroup
    "Miscellaneous options"
    [ testCleanOn,
      testCleanOff,
      testConfig,
      testConfigOff
    ]

testCleanOn :: TestTree
testCleanOn = testProp1 desc "testCleanOn" $ do
  testGetArgs e args
  where
    desc = "Parses --testClean on"

    args =
      [ "--clean",
        "on",
        "search-commit",
        commitStr
      ]

    e = set' (#coreConfig % #clean) (Just True) defArgs

testCleanOff :: TestTree
testCleanOff = testProp1 desc "testCleanOff" $ do
  testGetArgs e args
  where
    desc = "Parses --testCleanOff"

    args =
      [ "--clean",
        "off",
        "search-commit",
        commitStr
      ]

    e = set' (#coreConfig % #clean) (Just False) defArgs

testConfig :: TestTree
testConfig = testProp1 desc "testConfig" $ do
  testGetArgs e args
  where
    desc = "Parses --config path"

    args =
      [ "--config",
        unsafeDecode path,
        "search-commit",
        commitStr
      ]

    path = [ospPathSep|path/to/config|]

    e = set' #config (Just $ With path) defArgs

testConfigOff :: TestTree
testConfigOff = testProp1 desc "testConfigOff" $ do
  testGetArgs e args
  where
    desc = "Parses --config off"

    args =
      [ "--config",
        "off",
        "search-commit",
        commitStr
      ]

    e = set' #config (Just Disabled) defArgs

testGetArgs :: Args -> [String] -> PropertyT IO ()
testGetArgs expected args = do
  result <- liftIO $ runGetArgs args
  expected === result

runGetArgs :: [String] -> IO Args
runGetArgs args =
  Env.withArgs args
    . runEff
    . runOptparse
    $ Args.getArgs

defArgs :: Args
defArgs =
  MkArgs
    { command = SearchCommit commit,
      config = Nothing,
      coreConfig =
        MkConfig
          { auth = Nothing,
            clean = Nothing,
            logColor = Nothing,
            logLevel = Nothing,
            repo =
              MkRepoConfig
                { branches = Nothing,
                  domain = Nothing,
                  name = Nothing,
                  path = Nothing,
                  protocol = Nothing,
                  remoteName = Nothing
                }
          }
    }

commit :: Commit
commit = MkCommit [osstr|1234567|]

commitStr :: String
commitStr = "1234567"
