module Git.Search.Logging.Data
  ( LogLevel (..),
    mkLevelLog,
    mkSuccessLog,
  )
where

import Git.Search.Prelude

data LogLevel
  = LogLevelDebug
  | LogLevelInfo
  | LogLevelCommand
  | LogLevelWarn
  deriving stock (Eq, Ord, Show)

instance DecodeTOML LogLevel where
  tomlDecoder =
    tomlDecoder @Text >>= \case
      "debug" -> pure LogLevelDebug
      "info" -> pure LogLevelInfo
      "command" -> pure LogLevelCommand
      "warn" -> pure LogLevelWarn
      other -> fail $ "Unrecognized log-level" ++ show other

mkLevelLog :: Bool -> LogLevel -> String -> String
mkLevelLog color lvl s =
  mkColorFn color (levelToColor lvl) (levelToHeader lvl <> s)

mkSuccessLog :: Bool -> String -> String
mkSuccessLog color = mkColorFn color green

mkColorFn :: Bool -> String -> String -> String
mkColorFn True colorCode = \s -> colorCode <> s <> endCode
mkColorFn False _ = id

levelToHeader :: LogLevel -> String
levelToHeader = \case
  LogLevelDebug -> "[Debug]: "
  LogLevelInfo -> "[Info]: "
  LogLevelCommand -> "[Command]: "
  LogLevelWarn -> "[Warn]: "

levelToColor :: LogLevel -> String
levelToColor = \case
  LogLevelDebug -> gray
  LogLevelInfo -> blue
  LogLevelCommand -> magenta
  LogLevelWarn -> yellow

blue :: String
blue = "\ESC[34m"

green :: String
green = "\ESC[32m"

magenta :: String
magenta = "\ESC[35m"

yellow :: String
yellow = "\ESC[33m"

gray :: String
gray = "\ESC[90m"

endCode :: String
endCode = "\ESC[0m"
