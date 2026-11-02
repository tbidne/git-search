module Git.Search.Logging
  ( logDebug,
    logInfo,
    logCommand,
    logWarn,
    logSuccess,
    logWithLevel,
  )
where

import Git.Search.Config (Env (coreConfig))
import Git.Search.Config.Data (Config (logColor, logLevel))
import Git.Search.Logging.Data
  ( LogLevel (LogLevelCommand, LogLevelDebug, LogLevelInfo, LogLevelWarn),
    mkLevelLog,
    mkSuccessLog,
  )
import Git.Search.Prelude

logDebug ::
  ( HasCallStack,
    Reader Env :> es,
    Terminal :> es,
    Time :> es
  ) =>
  Text ->
  Eff es ()
logDebug = logWithLevel LogLevelDebug

logInfo ::
  ( HasCallStack,
    Reader Env :> es,
    Terminal :> es,
    Time :> es
  ) =>
  Text ->
  Eff es ()
logInfo = logWithLevel LogLevelInfo

logCommand ::
  ( HasCallStack,
    Reader Env :> es,
    Terminal :> es,
    Time :> es
  ) =>
  Text ->
  Eff es ()
logCommand = logWithLevel LogLevelCommand

logWarn ::
  ( HasCallStack,
    Reader Env :> es,
    Terminal :> es,
    Time :> es
  ) =>
  Text ->
  Eff es ()
logWarn = logWithLevel LogLevelWarn

logSuccess ::
  ( HasCallStack,
    Reader Env :> es,
    Terminal :> es
  ) =>
  Text ->
  Eff es ()
logSuccess t = do
  env <- ask @Env
  putTextLn $ mkSuccessLog env.coreConfig.logColor t

logWithLevel ::
  ( HasCallStack,
    Reader Env :> es,
    Terminal :> es,
    Time :> es
  ) =>
  LogLevel ->
  Text ->
  Eff es ()
logWithLevel logLvl t = do
  env <- ask @Env
  case env.coreConfig.logLevel of
    Nothing -> pure ()
    Just cfgLvl -> do
      when (cfgLvl <= logLvl) $ do
        putTextLn =<< mkLevelLog env.coreConfig.logColor logLvl t
