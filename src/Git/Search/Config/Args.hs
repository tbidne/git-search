{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

module Git.Search.Config.Args
  ( Args (..),
    getArgs,
  )
where

import Data.List qualified as L
import Data.Text qualified as T
import Data.Version (showVersion)
import Effectful.Optparse.Static qualified as EOA
import Git.Search.Config.Args.TH qualified as TH
import Git.Search.Config.Data
  ( Command (DeleteCache, SearchCommit),
    Commit (MkCommit),
    Config (MkConfig, branches, clean, logColor, logLevel, repo),
    ConfigPhase (ConfigPhaseArgs),
    Protocol (ProtocolHttps, ProtocolSsh),
    RepoConfig (MkRepoConfig, domain, name, protocol),
  )
import Git.Search.Config.WithDisabled (WithDisabled)
import Git.Search.Config.WithDisabled qualified as WD
import Git.Search.Logging.Data (LogLevel (LogLevelDebug, LogLevelInfo))
import Git.Search.Prelude
import Options.Applicative
  ( CommandFields,
    InfoMod,
    Mod,
    OptionFields,
    Parser,
    ParserInfo
      ( ParserInfo,
        infoFailureCode,
        infoFooter,
        infoFullDesc,
        infoHeader,
        infoParser,
        infoPolicy,
        infoProgDesc
      ),
    (<**>),
  )
import Options.Applicative qualified as OA
import Options.Applicative.Help (Doc)
import Options.Applicative.Help.Chunk (Chunk (Chunk))
import Options.Applicative.Help.Chunk qualified as Chunk
import Options.Applicative.Help.Pretty qualified as Pretty
import Options.Applicative.Types (ArgPolicy (Intersperse))
import Paths_git_search qualified as Paths
import System.Info qualified as Info

data Args = MkArgs
  { command :: Command ConfigPhaseArgs,
    config :: Maybe (WithDisabled OsPath),
    coreConfig :: Config ConfigPhaseArgs
  }

getArgs :: (Optparse :> es) => Eff es Args
getArgs = EOA.execParser parserInfoArgs
  where
    parserInfoArgs =
      ParserInfo
        { infoParser = argsParser,
          infoFullDesc = True,
          infoProgDesc = desc,
          infoHeader = Chunk headerTxt,
          infoFooter = Chunk footerTxt,
          infoFailureCode = 1,
          infoPolicy = Intersperse
        }
    headerTxt = Just "Git-search: Searches git repositories."
    footerTxt = Just $ fromString versShort

    desc =
      Chunk.vsepChunks
        [ Chunk.paragraph
            $ mconcat
              [ "Git-search allows searching a remote repository for info. ",
                "In general, the repository is cloned to a cache the first ",
                "time, so that subsequent runs are faster."
              ],
          Chunk.paragraph "Examples:",
          mkExample
            [ "1. Search a git repository for branches with a commit hash:",
              "",
              "$ git-search --name nixos/nixpkgs search-commit c190319",
              "Found 14 branches:",
              " - origin/master",
              " - origin/nixos-unstable",
              " - origin/nixos-unstable-small",
              " ..."
            ],
          mkExample
            [ "2. Filtering via --branches:",
              "",
              "$ git-search --name nixos/nixpkgs --branches '*master *unstable' search-commit c190319",
              "Found 3 branches:",
              " - origin/master",
              " - origin/nixos-unstable",
              " - origin/nixpkgs-unstable"
            ]
        ]

argsParser :: Parser Args
argsParser = do
  p
    <**> version
    <**> OA.helper
  where
    p = do
      ~(branches, domain, name, protocol) <- parseRepo

      ~(clean, config, logColor, logLevel) <- parseMisc

      command <- commandParser

      pure
        $ MkArgs
          { command,
            config,
            coreConfig =
              MkConfig
                { branches,
                  clean,
                  logColor,
                  logLevel,
                  repo =
                    MkRepoConfig
                      { domain,
                        name,
                        protocol
                      }
                }
          }

    parseRepo =
      OA.parserOptionGroup "Repository options:"
        $ (,,,)
        <$> branchesParser
        <*> domainParser
        <*> nameParser
        <*> protocolParser

    parseMisc =
      OA.parserOptionGroup "Miscellaneous options:"
        $ (,,,)
        <$> cleanParser
        <*> configParser
        <*> logColorParser
        <*> logLevelParser

branchesParser :: Parser (Maybe (WithDisabled [OsString]))
branchesParser =
  OA.optional
    $ OA.option
      r
    $ mconcat
      [ OA.long "branches",
        OA.metavar "STR",
        mkHelp
          $ mconcat
            [ "Filters the search via space-separated branches e.g. ",
              "'*master *some-branch'. Note that the searched branches includes ",
              "the remote (origin), so a prefix '*' may be desired."
            ]
      ]
  where
    r = do
      s <- OA.str
      WD.disabledParser s $ do
        let strs = fmap T.strip . T.words $ s
        traverse (encodeFail . unpack) strs

cleanParser :: Parser (Maybe Bool)
cleanParser =
  switchParser
    $ mconcat
      [ OA.long "clean",
        mkHelp
          $ mconcat
            [ "Performs a clean clone of the repo, overwriting any previous ",
              "clones. Otherwise runs 'fetch' if the repo has been ",
              "previously cloned."
            ]
      ]

commandParser :: Parser (Command ConfigPhaseArgs)
commandParser =
  OA.hsubparser
    ( mconcat
        [ mkCommand "search-commit" searchCommitParser searchCommitHelp,
          OA.commandGroup "Search commands:"
        ]
    )
    <|> OA.hsubparser
      ( mconcat
          [ mkCommand "delete-cache" deleteCacheParser deleteCacheHelp,
            OA.commandGroup "Miscellaneous commands:"
          ]
      )
  where
    searchCommitParser = SearchCommit <$> commitParser
    searchCommitHelp = mkCmdDescStrNoLine "Searches for a commit."

    deleteCacheParser = pure (DeleteCache ())
    deleteCacheHelp = mkCmdDescStr "Deletes the cache."

configParser :: Parser (Maybe (WithDisabled OsPath))
configParser =
  OA.optional
    $ OA.option
      r
    $ mconcat
      [ OA.long "config",
        OA.metavar "(PATH | off)",
        mkHelp
          $ mconcat
            [ "Path to TOML config. We also look in XDG config e.g. ",
              "~/.config/git-search/config.toml."
            ]
      ]
  where
    r = do
      s <- OA.str
      WD.disabledParser s $ encodeValidFail (unpack s)

domainParser :: Parser (Maybe (WithDisabled OsString))
domainParser =
  OA.optional
    $ OA.option
      r
    $ mconcat
      [ OA.long "domain",
        OA.metavar "(STR | off)",
        mkHelp "Repository domain. Defaults to github.com."
      ]
  where
    r = do
      s <- OA.str
      WD.disabledParser s (encodeFail . unpack $ s)

logColorParser :: Parser (Maybe Bool)
logColorParser =
  switchParser
    $ mconcat
      [ OA.long "log-color",
        mkHelp "Enables log colors. Defaults to 'on'."
      ]

logLevelParser :: Parser (Maybe (WithDisabled LogLevel))
logLevelParser =
  OA.optional
    $ OA.option
      r
    $ mconcat
      [ OA.long "log-level",
        OA.metavar "(debug | info | off)",
        mkHelpNoLine "Enables logging."
      ]
  where
    r = do
      s <- OA.str
      WD.disabledParser s $ case s of
        "debug" -> pure LogLevelDebug
        "info" -> pure LogLevelInfo
        other -> fail $ "Unrecognized log-level: " ++ show other

commitParser :: Parser Commit
commitParser =
  OA.argument
    (MkCommit <$> osString)
    $ mconcat
      [ OA.metavar "HASH",
        mkHelp "Commit hash for which we want to search."
      ]

nameParser :: Parser (Maybe (WithDisabled OsString))
nameParser =
  OA.optional
    $ OA.option
      r
    $ mconcat
      [ OA.long "name",
        OA.metavar "STR",
        mkHelp
          $ mconcat
            [ "Repository name. This should be the organization and repo ",
              "following github.com e.g. nixos/nixpkgs for ",
              "github.com/nixos/nixpkgs. Mutually exclusive with --repo."
            ]
      ]
  where
    r = do
      s <- OA.str
      WD.disabledParser s (encodeFail . unpack $ s)

protocolParser :: Parser (Maybe (WithDisabled Protocol))
protocolParser =
  OA.optional
    $ OA.option
      r
    $ mconcat
      [ OA.long "protocol",
        OA.metavar "(https | ssh | off)",
        mkHelpNoLine "Protocol to use. Defaults to https."
      ]
  where
    r = do
      s <- OA.str
      WD.disabledParser s $ case s of
        "https" -> pure ProtocolHttps
        "ssh" -> pure ProtocolSsh
        other -> fail $ "Unknown protocol: " ++ unpack other

version :: Parser (a -> a)
version = OA.infoOption versLong (OA.long "version" <> OA.short 'v' <> OA.hidden)

versShort :: String
versShort =
  mconcat
    [ "Git-search: ",
      showVersion Paths.version,
      " (",
      decodeLenient versionInfo.gitShortHash,
      ")"
    ]

versLong :: String
versLong =
  L.intercalate
    "\n"
    [ "Git-search: " <> showVersion Paths.version,
      " - Git revision: " <> decodeLenient versionInfo.gitHash,
      " - Commit date:  " <> decodeLenient versionInfo.gitCommitDate,
      " - GHC version:  " <> versionInfo.ghc
    ]

data VersionInfo = MkVersionInfo
  { gitCommitDate :: OsString,
    ghc :: String,
    gitHash :: OsString,
    gitShortHash :: OsString
  }

versionInfo :: VersionInfo
versionInfo =
  MkVersionInfo
    { gitCommitDate = d,
      ghc = showVersion Info.fullCompilerVersion,
      gitHash = h,
      gitShortHash = sh
    }
  where
    (d, h, sh) = $$TH.gitData

mkHelp :: String -> Mod f a
mkHelp =
  OA.helpDoc
    . fmap (<> Pretty.hardline)
    . Chunk.unChunk
    . Chunk.paragraph

mkHelpNoLine :: String -> Mod f a
mkHelpNoLine =
  OA.helpDoc
    . Chunk.unChunk
    . Chunk.paragraph

switchParser :: Mod OptionFields Bool -> Parser (Maybe Bool)
switchParser mods =
  OA.optional
    $ OA.option
      r
      mods'
  where
    r =
      OA.str >>= \case
        "off" -> pure False
        "on" -> pure True
        other -> fail $ "Unrecognized: " ++ other

    mods' = mods <> OA.metavar "(on | off)"

mkCommand :: String -> Parser a -> InfoMod a -> Mod CommandFields a
mkCommand cmdTxt parser helpTxt = OA.command cmdTxt (OA.info parser helpTxt)

mkCmdDescStr :: String -> InfoMod a
mkCmdDescStr = mkCmdDesc . Chunk.paragraph

mkCmdDescStrNoLine :: String -> InfoMod a
mkCmdDescStrNoLine = mkCmdDescNoLine . Chunk.paragraph

mkCmdDesc :: Chunk Doc -> InfoMod a
mkCmdDesc =
  OA.progDescDoc
    . fmap (<> Pretty.hardline)
    . Chunk.unChunk

-- For the last command, so we do not append two lines (there is an automatic
-- one at the end).
mkCmdDescNoLine :: Chunk Doc -> InfoMod a
mkCmdDescNoLine =
  OA.progDescDoc
    . Chunk.unChunk

mkExample :: NonEmpty String -> Chunk Doc
mkExample = identPara 2 5

identPara :: Int -> Int -> NonEmpty String -> Chunk Doc
identPara hIndent lIndent (h :| xs) =
  Chunk.vcatChunks
    . (\ys -> toChunk hIndent h : ys)
    . fmap (toChunk lIndent)
    $ xs

toChunk :: Int -> String -> Chunk Doc
toChunk _ "" = line
toChunk i other = fmap (Pretty.indent i) . Chunk.stringChunk $ other

line :: Chunk Doc
line = Chunk (Just Pretty.softline)
