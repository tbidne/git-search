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
    Config (MkConfig, branches, clean, debug, repo),
    ConfigPhase (ConfigPhaseArgs),
    Protocol (ProtocolHttps, ProtocolSsh),
    RepoConfig (MkRepoConfig, domain, name, protocol),
    WithDisabled (Disabled, With),
  )
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
    headerTxt = Just "Git-search: Searches a git repository for commit hashes."
    footerTxt = Just $ fromString versShort

    desc =
      Chunk.vsepChunks
        [ Chunk.paragraph
            $ mconcat
              [ "Given a git repository and commit hash, returns a list of ",
                "branches containing that hash."
              ],
          Chunk.paragraph
            $ mconcat
              [ "Initially, the git repository is cloned and cached. Subsequent ",
                "searches invoke 'fetch' for performance."
              ],
          Chunk.paragraph "Examples:",
          mkExample
            [ "1. Running for the first time:",
              "",
              "$ git-search --name nixos/nixpkgs search-commit c190319",
              "Cloning https://github.com/nixos/nixpkgs...",
              "Clone finished: 8 minutes, 55 seconds",
              "Searching for hash f61423d...",
              "Search finished: 3 minutes, 57 seconds",
              "Found 14 branches:",
              " - origin/master",
              " - origin/nixos-unstable",
              " - origin/nixos-unstable-small",
              " ..."
            ],
          mkExample
            [ "2. Running a second time, using the cache:",
              "",
              "$ git-search --name nixos/nixpkgs search-commit c190319",
              "Fetching https://github.com/nixos/nixpkgs...",
              "Fetch finished: 1 second",
              "...",
              "Found 14 branches:",
              " - origin/master",
              " - origin/nixos-unstable",
              " - origin/nixos-unstable-small",
              " ..."
            ],
          mkExample
            [ "3. Filtering via --branches:",
              "",
              "$ git-search --name nixos/nixpkgs --branches '*master *unstable' search-commit c190319",
              "...",
              "Found 3 branches:",
              " - origin/master",
              " - origin/nixos-unstable",
              " - origin/nixpkgs-unstable"
            ]
        ]

    mkExample :: NonEmpty String -> Chunk Doc
    mkExample = identPara 2 5

    identPara :: Int -> Int -> NonEmpty String -> Chunk Doc
    identPara hIndent lIndent (h :| xs) =
      Chunk.vcatChunks
        . (\ys -> toChunk hIndent h : ys)
        . fmap (toChunk lIndent)
        $ xs

    toChunk _ "" = line
    toChunk i other = fmap (Pretty.indent i) . Chunk.stringChunk $ other

    line = Chunk (Just Pretty.softline)

argsParser :: Parser Args
argsParser = do
  p
    <**> version
    <**> OA.helper
  where
    p = do
      ~(branches, domain, name, protocol) <- parseRepo

      ~(clean, config, debug) <- parseMisc

      command <- commandParser

      pure
        $ MkArgs
          { command,
            config,
            coreConfig =
              MkConfig
                { branches,
                  clean,
                  debug,
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
        $ (,,)
        <$> cleanParser
        <*> configParser
        <*> debugParser

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
      case s of
        "off" -> pure Disabled
        other -> do
          let strs = fmap T.strip . T.words $ other
          With <$> traverse (encodeFail . unpack) strs

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
      case s of
        "off" -> pure Disabled
        other -> With <$> encodeValidFail other

debugParser :: Parser (Maybe Bool)
debugParser =
  switchParser
    $ mconcat
      [ OA.long "debug",
        mkHelpNoLine "Enables additional logging."
      ]

domainParser :: Parser (Maybe OsString)
domainParser =
  OA.optional
    $ OA.option
      osString
    $ mconcat
      [ OA.long "domain",
        OA.metavar "STR",
        mkHelp "Repository domain. Defaults to github.com."
      ]

commitParser :: Parser Commit
commitParser =
  OA.argument
    (MkCommit <$> osString)
    $ mconcat
      [ OA.metavar "HASH",
        mkHelp "Commit hash for which we want to search."
      ]

nameParser :: Parser (Maybe OsString)
nameParser =
  OA.optional
    $ OA.option
      osString
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

protocolParser :: Parser (Maybe Protocol)
protocolParser =
  OA.optional
    $ OA.option
      r
    $ mconcat
      [ OA.long "protocol",
        OA.metavar "(https | ssh)",
        mkHelpNoLine "Protocol to use. Defaults to https."
      ]
  where
    r =
      OA.str >>= \case
        "https" -> pure ProtocolHttps
        "ssh" -> pure ProtocolSsh
        other -> fail $ "Unknown protocol: " ++ other

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
