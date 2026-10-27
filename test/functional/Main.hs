{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Data.IORef (IORef)
import Data.IORef qualified as IORef
import Data.List qualified as L
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T
import Effectful.Dispatch.Dynamic qualified as Eff.Dyn
import Effectful.FileSystem.HandleWriter.Dynamic qualified as HW
import Effectful.Terminal.Dynamic qualified as Term
import GHC.Clock qualified as CC
import Git.Search.Prelude
import Git.Search.Runner qualified
import System.Environment qualified as Env
import System.IO qualified as IO
import Test.Tasty (DependencyType (AllSucceed), TestTree, dependentTestGroup)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit (assertBool, assertEqual, testCase, (@=?))

main :: IO ()
main = do
  Env.lookupEnv "TEST_FUNCTIONAL" >>= \case
    Nothing -> dontRun
    Just cmd -> do
      cloneTimeRef <- IORef.newIORef Nothing

      mAuth <- Env.lookupEnv "GH_TOKEN"

      case T.toLower (T.strip (pack cmd)) of
        "clone" ->
          runTests
            $ testNixpkgsCommitClone cloneTimeRef
            : mainTests cloneTimeRef mAuth
        _ -> runTests (mainTests cloneTimeRef mAuth)
  where
    dontRun = IO.putStrLn "*** Functional tests disabled. Enable with TEST_FUNCTIONAL=1 ***"

    runTests tests =
      Tasty.defaultMain
        $ dependentTestGroup
          "Functional"
          AllSucceed
          tests

    mainTests ref mAuth =
      [ testNixpkgsCommitFetch ref,
        testNixpkgsCommitBranches,
        testNixpkgsDefaults mAuth,
        testNixpkgsPullRequest mAuth
      ]

testNixpkgsCommitClone :: IORef (Maybe Double) -> TestTree
testNixpkgsCommitClone cloneTimeRef = testCase desc $ do
  start <- CC.getMonotonicTime
  results <- Set.fromList <$> runSearch args
  end <- CC.getMonotonicTime
  assertResults fullExpected results

  let diff = end - start
  IORef.writeIORef cloneTimeRef (Just diff)
  where
    desc = "Searches commit in nixos/nixpkgs with clean"

    args = "--clean" : "on" : argsToStrs mempty

testNixpkgsCommitFetch :: IORef (Maybe Double) -> TestTree
testNixpkgsCommitFetch cloneTimeRef = testCase desc $ do
  start2 <- CC.getMonotonicTime
  results <- Set.fromList <$> runSearch (argsToStrs mempty)
  end2 <- CC.getMonotonicTime
  assertResults fullExpected results

  -- If diff1 was saved (first test run), then test the time diff.
  mDiff1 <- IORef.readIORef cloneTimeRef
  for_ mDiff1 $ \diff1 -> do
    -- Assert 2nd run at least 2x faster than 1st run.
    let diff2 = end2 - start2
        timeErr =
          mconcat
            [ "Expected 2nd run (",
              show diff2,
              ") >= 2x 1st run (",
              show diff1,
              ")."
            ]
    assertBool timeErr (diff2 * 2 < diff1)
  where
    desc = "Searches commit in nixos/nixpkgs"

testNixpkgsCommitBranches :: TestTree
testNixpkgsCommitBranches = testCase desc $ do
  results <- runSearch (argsToStrs argsCfg)
  expected @=? results
  where
    desc = "Searches commit in nixos/nixpkgs with branch filters"

    expected =
      [ "- origin/master",
        "- origin/nixos-unstable",
        "- origin/nixos-unstable-small",
        "- origin/nixpkgs-unstable"
      ]

testNixpkgsDefaults :: Maybe String -> TestTree
testNixpkgsDefaults mAuth = testCase desc $ do
  results <- mkResultsMap <$> runSearch args
  assertResultsN expected results
  where
    desc = "Searches defaults in nixos/nixpkgs"

    args = case mAuth of
      Nothing -> baseArgs
      Just auth -> "--auth" : auth : baseArgs

    baseArgs =
      ["--branches", branches]
        ++ (argsToStrs $ argsCfg {cmd = Just ["search-defaults"]})

    expected =
      [ -- Both results
        ("- origin/haskell-updates", 2),
        ("- origin/master", 2),
        ("- origin/nixos-unstable", 2),
        ("- origin/nixos-unstable-small", 2),
        ("- origin/nixpkgs-unstable", 2),
        -- PR results
        ("- origin/nixpkgs-26.05-darwin", 1)
      ]

    branches = "*master *unstable* *haskell* *darwin"

testNixpkgsPullRequest :: Maybe String -> TestTree
testNixpkgsPullRequest mAuth = testCase desc $ do
  results <- Set.fromList <$> runSearch args
  assertResults expected results
  where
    desc = "Searches pr in nixos/nixpkgs"

    args = case mAuth of
      Nothing -> baseArgs
      Just auth -> "--auth" : auth : baseArgs

    baseArgs =
      argsToStrs $ mempty {cmd = Just ["search-pr", "510883"]}

    expected =
      [ "- origin/haskell-updates",
        "- origin/master",
        "- origin/nixos-unstable",
        "- origin/nixos-unstable-small",
        "- origin/nixpkgs-26.05-darwin",
        "- origin/nixpkgs-unstable"
      ]

data Args = MkArgs
  { cmd :: Maybe [String],
    cfg :: Maybe OsString,
    name :: Maybe String
  }

instance Semigroup Args where
  MkArgs x1 x2 x3 <> MkArgs y1 y2 y3 =
    MkArgs (x1 <|> y1) (x2 <|> y2) (x3 <|> y3)

instance Monoid Args where
  mempty = MkArgs Nothing Nothing Nothing

argsCfg :: Args
argsCfg =
  MkArgs
    { cmd = Nothing,
      cfg = Just [osstr|examples/config.toml|],
      name = Just "nix"
    }

argsToStrs :: Args -> [String]
argsToStrs args =
  cfgArgs
    ++ otherArgs
    ++ nameArgs
    ++ cmdArgs
  where
    cfgArgs = case args.cfg of
      Nothing -> ["--config", "off"]
      Just cfgPath -> ["--config", unsafeDecode cfgPath]

    cmdArgs =
      fromMaybe
        ["search-commit", "c190319055bb5c31acfd7bb8356ce9ab05cb2b36"]
        args.cmd

    nameArgs =
      maybe
        ["--name", "nixos/nixpkgs"]
        (\n -> ["--name", n])
        args.name

    otherArgs =
      [ "--log-color",
        "off",
        "--log-level",
        "debug"
      ]

fullExpected :: [Text]
fullExpected =
  [ "- origin/haskell-updates",
    "- origin/master",
    "- origin/nixos-unstable",
    "- origin/nixos-unstable-small",
    "- origin/nixpkgs-unstable",
    "- origin/staging",
    "- origin/staging-next",
    "- origin/staging-nixos"
  ]

assertResults :: [Text] -> Set Text -> IO ()
assertResults expected rs = for_ expected $ \e -> do
  let isMember = Set.member e rs
      err =
        mconcat
          [ "Expected branch '",
            unpack e,
            "' in results:",
            unpack
              . mconcat
              . fmap ("\n" <>)
              $ Set.toList rs
          ]
  assertBool err isMember

assertResultsN :: [(Text, Int)] -> Map Text Int -> IO ()
assertResultsN expected rs = for_ expected $ \(eTxt, eN) -> do
  let actualN = Map.findWithDefault 0 eTxt rs
      err =
        mconcat
          [ "Expected branch '",
            unpack eTxt,
            "' ",
            show eN,
            " times in results:",
            unpack
              . mconcat
              . fmap (("\n" <>) . (\(t, n) -> t <> ": " <> pack (show n)))
              $ Map.toList rs
          ]
  assertEqual err eN actualN

mkResultsMap :: [Text] -> Map Text Int
mkResultsMap = L.foldl' go Map.empty
  where
    go mp t = Map.alter k t mp

    k = \case
      Nothing -> Just 1
      Just n -> let !n' = n + 1 in Just n'

runSearch :: [String] -> IO [Text]
runSearch args = do
  logsRef <- IORef.newIORef []

  eResult <-
    trySync
      $ Env.withArgs args
      . runEff
      . runConcurrent
      . runFileReader
      . runHandleReader
      . runHandleWriter
      . runHandleW
      . runNetwork
      . runPathReader
      . runPathWriter
      . runOptparse
      . runProcess
      . runTerm logsRef
      . runTime
      $ Git.Search.Runner.runSearch

  fullLogs <- fmap T.strip <$> IORef.readIORef logsRef

  let logs = L.filter (T.isPrefixOf "- ") fullLogs

  case eResult of
    Left ex -> do
      IO.putStrLn
        $ mconcat
          [ "Logs: ",
            unpack $ T.intercalate "\n" fullLogs,
            "\n"
          ]
      throwIO ex
    Right () -> pure logs

runTerm ::
  (IOE :> es) =>
  IORef [Text] ->
  Eff (Terminal : es) a ->
  Eff es a
runTerm logsRef = interpret_ $ \case
  Term.PutStrLn str -> do
    -- Lines so that we split the log with results into separate lines.
    let strs = T.lines $ pack str
    liftIO $ IORef.modifyIORef logsRef (<> strs)
  other -> error $ "unimplemented: " ++ showEffectCons other

-- Override HandleWriter to prevent test output interference.
runHandleW ::
  (HandleWriter :> es) =>
  Eff es a ->
  Eff es a
runHandleW = Eff.Dyn.interpose $ \env -> \case
  HW.HSetBuffering _ _ -> pure ()
  HW.HSetEcho _ _ -> pure ()
  op -> Eff.Dyn.passthrough env op
