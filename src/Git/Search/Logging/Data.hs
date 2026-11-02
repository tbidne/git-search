module Git.Search.Logging.Data
  ( LogLevel (..),
    mkLevelLog,
    mkSuccessLog,
  )
where

import Data.Text.Builder.Linear (Builder)
import Data.Text.Builder.Linear qualified as TBL
import Effectful.Time.Static (getSystemTimeString)
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

mkLevelLog ::
  ( HasCallStack,
    Time :> es
  ) =>
  Bool ->
  LogLevel ->
  Text ->
  Eff es Text
mkLevelLog color lvl msg = do
  ts <- getTimestampHeader
  pure $ TBL.runBuilder $ colorFn (ts <> levelToHeader lvl <> msgBuilder)
  where
    msgBuilder = TBL.fromText msg
    colorFn = mkColorFn color (levelToColor lvl)

mkSuccessLog :: Bool -> Text -> Text
mkSuccessLog color = TBL.runBuilder . mkColorFn color green . TBL.fromText

mkColorFn :: Bool -> Builder -> Builder -> Builder
mkColorFn True colorCode = \s -> colorCode <> s <> endCode
mkColorFn False _ = id

getTimestampHeader :: (HasCallStack, Time :> es) => Eff es Builder
getTimestampHeader = brackets . fromString <$> getSystemTimeString

levelToHeader :: LogLevel -> Builder
levelToHeader = \case
  LogLevelDebug -> "[Debug]: "
  LogLevelInfo -> "[Info]:  "
  LogLevelCommand -> "[Cmd]:   "
  LogLevelWarn -> "[Warn]:  "

levelToColor :: LogLevel -> Builder
levelToColor = \case
  LogLevelDebug -> gray
  LogLevelInfo -> blue
  LogLevelCommand -> magenta
  LogLevelWarn -> yellow

brackets :: Builder -> Builder
brackets s = "[" <> s <> "]"

blue :: Builder
blue = "\ESC[34m"

green :: Builder
green = "\ESC[32m"

magenta :: Builder
magenta = "\ESC[35m"

yellow :: Builder
yellow = "\ESC[33m"

gray :: Builder
gray = "\ESC[90m"

endCode :: Builder
endCode = "\ESC[0m"
