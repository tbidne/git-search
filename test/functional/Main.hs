{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Control.Exception (throwIO)
import Data.IORef (IORef)
import Data.IORef qualified as IORef
import Data.List qualified as L
import Data.Set qualified as Set
import Data.Text qualified as T
import Effectful.Dispatch.Dynamic (interpret_)
import Effectful.Dynamic.Utils (showEffectCons)
import Effectful.Terminal.Dynamic qualified as Term
import GHC.Clock qualified as CC
import Git.Search.Prelude
import Git.Search.Runner qualified
import System.Environment qualified as Env
import System.Environment.Guard (ExpectEnv (ExpectEnvSet), guardOrElse')
import System.IO qualified as IO
import Test.Tasty (TestTree, testGroup)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit (assertBool, testCase, (@=?))

main :: IO ()
main = guardOrElse' "TEST_FUNCTIONAL" ExpectEnvSet runTests dontRun
  where
    runTests =
      Tasty.defaultMain
        $ testGroup
          "Functional"
          [ testSearchNixpkgs
          ]

    dontRun = IO.putStrLn "*** Functional tests disabled. Enable with TEST_FUNCTIONAL=1 ***"

testSearchNixpkgs :: TestTree
testSearchNixpkgs = testCase "Finds branches in nixos/nixpkgs" $ do
  -- 1st run
  start1 <- CC.getMonotonicTime
  results1 <- Set.fromList <$> runSearch args1
  end1 <- CC.getMonotonicTime
  assertResults results1

  -- 2nd run
  start2 <- CC.getMonotonicTime
  results2 <- Set.fromList <$> runSearch (mkArgs [osp|off|])
  end2 <- CC.getMonotonicTime
  assertResults results2

  -- Assert 2nd run at least 2x faster than 1st run.
  let diff1 = end1 - start1
      diff2 = end2 - start2
      timeErr =
        mconcat
          [ "Expected 2nd run (",
            show diff2,
            ") >= 2x 1st run (",
            show diff1,
            ")."
          ]
  assertBool timeErr (diff2 * 2 < diff1)

  -- 3rd run, w/ config filtering.
  results3 <- runSearch (mkArgs [ospPathSep|examples/config.toml|])
  expected3 @=? results3
  where
    args1 = "--clean" : "on" : mkArgs [osp|off|]

    mkArgs cfgPath =
      [ "--config",
        decodeLenient cfgPath,
        "--debug",
        "on",
        "--name",
        "nixos/nixpkgs",
        "search-commit",
        "c190319055bb5c31acfd7bb8356ce9ab05cb2b36"
      ]

    assertResults rs = for_ expected $ \e -> do
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

    expected =
      [ "- origin/haskell-updates",
        "- origin/master",
        "- origin/nixos-unstable",
        "- origin/nixos-unstable-small",
        "- origin/nixpkgs-unstable",
        "- origin/staging",
        "- origin/staging-next",
        "- origin/staging-nixos"
      ]

    expected3 =
      [ "- origin/master",
        "- origin/nixos-unstable",
        "- origin/nixos-unstable-small",
        "- origin/nixpkgs-unstable"
      ]

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
