module Git.Search.Logging
  ( logDebug,
    logInfo,
    logSuccess,
  )
where

import Git.Search.Config (Env (coreConfig))
import Git.Search.Config.Data (Config (logColor, logLevel))
import Git.Search.Logging.Data
  ( LogLevel (LogLevelDebug, LogLevelInfo),
    mkLevelLog,
    mkSuccessLog,
  )
import Git.Search.Prelude

logDebug ::
  ( HasCallStack,
    Reader Env :> es,
    Terminal :> es
  ) =>
  String ->
  Eff es ()
logDebug = logWithLevel LogLevelDebug

logInfo ::
  ( HasCallStack,
    Reader Env :> es,
    Terminal :> es
  ) =>
  String ->
  Eff es ()
logInfo = logWithLevel LogLevelInfo

logSuccess ::
  ( HasCallStack,
    Reader Env :> es,
    Terminal :> es
  ) =>
  String ->
  Eff es ()
logSuccess t = do
  env <- ask @Env
  putStrLn $ mkSuccessLog env.coreConfig.logColor t

logWithLevel ::
  ( HasCallStack,
    Reader Env :> es,
    Terminal :> es
  ) =>
  LogLevel ->
  String ->
  Eff es ()
logWithLevel logLvl t = do
  env <- ask @Env
  case env.coreConfig.logLevel of
    Nothing -> pure ()
    Just cfgLvl -> do
      when (cfgLvl <= logLvl) $ do
        putStrLn $ mkLevelLog env.coreConfig.logColor logLvl t
