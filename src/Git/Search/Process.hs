{-# LANGUAGE QuasiQuotes #-}

module Git.Search.Process
  ( -- * Git
    runGit,
    runGitOut,

    -- * Process
    runProc,
    runProcOut,
  )
where

import Control.Exception.Utils qualified as Ex.Utils
import Data.List qualified as L
import Data.Text qualified as T
import Effectful.Process qualified as P
import Git.Search.Config (Env)
import Git.Search.Logging qualified as Logging
import Git.Search.Prelude

-- | Like 'runGit', but throws an exception on ExitFailure.
runGitOut ::
  ( HasCallStack,
    Process :> es,
    Reader Env :> es,
    Terminal :> es,
    Time :> es
  ) =>
  List OsString ->
  Eff es OsString
runGitOut = runProcOut [osstr|git|]

-- | Runs git process.
runGit ::
  ( HasCallStack,
    Process :> es,
    Reader Env :> es,
    Terminal :> es,
    Time :> es
  ) =>
  List OsString ->
  Eff es (ExitCode, String, String)
runGit = runProc [osstr|git|]

-- | Like 'runProc', but throws an exception on ExitFailure.
runProcOut ::
  ( HasCallStack,
    Process :> es,
    Reader Env :> es,
    Terminal :> es,
    Time :> es
  ) =>
  OsString ->
  List OsString ->
  Eff es OsString
runProcOut exe args = do
  let exeStr = decodeLenient exe
  (ec, out, err) <- runProc exe args
  case ec of
    ExitFailure _ -> do
      let argsStr = L.unwords $ fmap decodeLenient args
      Ex.Utils.throwString
        $ unpack
        $ mconcat
          [ "Error running process '",
            pack exeStr,
            "' with args '",
            pack argsStr,
            "': ",
            pack err
          ]
    ExitSuccess -> encodeThrowM out

-- | Runs a process.
runProc ::
  ( HasCallStack,
    Process :> es,
    Reader Env :> es,
    Terminal :> es,
    Time :> es
  ) =>
  OsString ->
  List OsString ->
  Eff es (ExitCode, String, String)
runProc exe args = do
  exeStr <- decodeThrowM exe
  argsStrs <- traverse decodeThrowM args

  let msg =
        mconcat
          [ "runProcess: ",
            T.unwords (pack exeStr : fmap pack argsStrs)
          ]

  Logging.logDebug msg

  P.readProcessWithExitCode exeStr argsStrs name
  where
    name = "runProcess"
