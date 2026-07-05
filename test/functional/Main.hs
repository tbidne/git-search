module Main (main) where

import Control.Exception (throwIO)
import Control.Exception.Utils (trySync)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_)
import Data.IORef (IORef)
import Data.IORef qualified as IORef
import Data.List qualified as L
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Effectful (Eff, IOE, (:>))
import Effectful qualified
import Effectful.Dispatch.Dynamic (interpret_)
import Effectful.Dynamic.Utils (showEffectCons)
import Effectful.FileSystem.PathReader.Static qualified as PR
import Effectful.FileSystem.PathWriter.Static qualified as PW
import Effectful.Optparse.Static qualified as EOA
import Effectful.Process qualified as P
import Effectful.Terminal.Dynamic (Terminal)
import Effectful.Terminal.Dynamic qualified as Term
import Effectful.Time.Static qualified as Time
import GHC.Clock qualified as CC
import Git.Search qualified
import Git.Search.Args qualified
import System.Environment qualified as Env
import System.Environment.Guard (ExpectEnv (ExpectEnvSet), guardOrElse')
import Test.Tasty (TestTree, testGroup)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit (assertBool, testCase)

main :: IO ()
main = guardOrElse' "TEST_FUNCTIONAL" ExpectEnvSet runTests dontRun
  where
    runTests =
      Tasty.defaultMain $
        testGroup
          "Functional"
          [ testSearchNixpkgs
          ]

    dontRun = putStrLn "*** Functional tests disabled. Enable with TEST_FUNCTIONAL=1 ***"

testSearchNixpkgs :: TestTree
testSearchNixpkgs = testCase "Finds branches in nixos/nixpkgs" $ do
  start1 <- CC.getMonotonicTime
  results1 <- Set.fromList <$> searchPrint args1
  end1 <- CC.getMonotonicTime
  assertResults results1

  start2 <- CC.getMonotonicTime
  results2 <- Set.fromList <$> searchPrint args2
  end2 <- CC.getMonotonicTime
  assertResults results2

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
  where
    args1 = "--clean" : args2

    args2 =
      [ "--debug",
        "--hash",
        "c190319055bb5c31acfd7bb8356ce9ab05cb2b36",
        "--name",
        "nixos/nixpkgs"
      ]

    assertResults rs = for_ expected $ \e -> do
      let isMember = Set.member e rs
          err =
            mconcat
              [ "Expected branch '",
                T.unpack e,
                "' in results:",
                T.unpack $ mconcat $ fmap ("\n - " <>) (Set.toList rs)
              ]
      assertBool err isMember

    expected =
      [ "origin/haskell-updates",
        "origin/master",
        "origin/nixos-unstable",
        "origin/nixos-unstable-small",
        "origin/nixpkgs-unstable",
        "origin/staging",
        "origin/staging-next",
        "origin/staging-nixos"
      ]

    searchPrint args = do
      logsRef <- IORef.newIORef []

      eResult <-
        trySync
          $ Env.withArgs args
            . Effectful.runEff
            . PR.runPathReader
            . PW.runPathWriter
            . EOA.runOptparse
            . P.runProcess
            . runTerminal logsRef
            . Time.runTime
          $ Git.Search.search
            =<< Git.Search.Args.getArgs

      case eResult of
        Left ex -> do
          logs <- L.reverse <$> IORef.readIORef logsRef
          putStrLn $
            mconcat
              [ "Logs: ",
                T.unpack $ T.intercalate "\n" logs,
                "\n"
              ]
          throwIO ex
        Right r -> pure r

runTerminal ::
  (IOE :> es) =>
  IORef [Text] ->
  Eff (Terminal : es) a ->
  Eff es a
runTerminal logsRef = interpret_ $ \case
  Term.PutStrLn str ->
    liftIO $ IORef.modifyIORef logsRef (T.pack str :)
  other -> error $ "unimplemented: " ++ showEffectCons other
