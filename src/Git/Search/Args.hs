{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

module Git.Search.Args
  ( getArgs,
  )
where

import Data.List qualified as L
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.String (IsString (fromString))
import Data.Text qualified as T
import Data.Version (showVersion)
import Effectful (Eff, (:>))
import Effectful.Optparse.Static (Optparse)
import Effectful.Optparse.Static qualified as EOA
import FileSystem.OsString (OsString)
import FileSystem.OsString qualified as FS.OsStr
import Git.Search.Args.TH qualified as TH
import Git.Search.Config
  ( Args,
    Config (MkConfig, branches, clean, commit, debug, repo),
    Protocol (ProtocolHttps, ProtocolSsh),
    RepoArgs (MkRepoArgs, domain, name, protocol),
  )
import Options.Applicative
  ( Mod,
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
        [ Chunk.paragraph $
            mconcat
              [ "Given a git repository and commit hash, returns a list of ",
                "branches containing that hash."
              ],
          Chunk.paragraph $
            mconcat
              [ "Initially, the git repository is cloned and cached. Subsequent ",
                "searches invoke 'fetch' for performance."
              ],
          Chunk.paragraph "Examples:",
          mkExample
            [ "1. Running for the first time:",
              "",
              "$ git-search --commit c190319 --name nixos/nixpkgs",
              "Cloning https://github.com/nixos/nixpkgs...",
              "Clone finished: 8 minutes, 55 seconds",
              "Searching for hash f61423d...",
              "Search finished: 3 minutes, 57 seconds",
              "Found branches:",
              " - origin/master",
              " - origin/nixos-unstable",
              " - origin/nixos-unstable-small",
              " ..."
            ],
          mkExample
            [ "2. Running a second time, using the cache:",
              "",
              "$ git-search --commit c190319 --name nixos/nixpkgs",
              "Fetching https://github.com/nixos/nixpkgs...",
              "Fetch finished: 1 second",
              "...",
              "Found branches:",
              " - origin/master",
              " - origin/nixos-unstable",
              " - origin/nixos-unstable-small",
              " ..."
            ],
          mkExample
            [ "3. Filtering via --branches:",
              "",
              "$ git-search --commit c190319 --name nixos/nixpkgs --branches '*master *unstable'",
              "...",
              "Found branches:",
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
      ~(commit, name) <- parseRequired

      ~(branches, domain, protocol) <- parseRepo

      ~(clean, debug) <- parseMisc

      pure $
        MkConfig
          { branches,
            clean,
            commit,
            debug,
            repo =
              MkRepoArgs
                { domain,
                  name,
                  protocol
                }
          }

    parseRequired =
      OA.parserOptionGroup "Required fields:" $
        (,)
          <$> commitParser
          <*> nameParser

    parseRepo =
      OA.parserOptionGroup "Repository options:" $
        (,,)
          <$> branchesParser
          <*> domainParser
          <*> protocolParser

    parseMisc =
      OA.parserOptionGroup "Miscellaneous options:" $
        (,)
          <$> cleanParser
          <*> debugParser

branchesParser :: Parser (Maybe [OsString])
branchesParser =
  OA.optional
    $ OA.option
      r
    $ mconcat
      [ OA.long "branches",
        OA.metavar "STR",
        mkHelp $
          mconcat
            [ "Filters the search via space-separated branches e.g. ",
              "'*master *some-branch'. Generally, branches should be prefixed ",
              "with a star to handle <remote>/<branch> syntax."
            ]
      ]
  where
    r = do
      strs <- fmap T.strip . T.words <$> OA.str
      traverse (FS.OsStr.encodeFail . T.unpack) strs

cleanParser :: Parser Bool
cleanParser =
  OA.switch $
    mconcat
      [ OA.long "clean",
        mkHelp $
          mconcat
            [ "Performs a clean clone of the repo, overwriting any previous ",
              "clones. Otherwise runs 'fetch' if the repo has been ",
              "previously cloned."
            ]
      ]

debugParser :: Parser Bool
debugParser =
  OA.switch $
    mconcat
      [ OA.long "debug",
        mkHelpNoLine "Enables additional logging."
      ]

domainParser :: Parser (Maybe OsString)
domainParser =
  OA.optional
    $ OA.option
      r
    $ mconcat
      [ OA.long "domain",
        OA.metavar "STR",
        mkHelp "Repository domain. Defaults to github.com."
      ]
  where
    r = OA.str >>= FS.OsStr.encodeFail

commitParser :: Parser OsString
commitParser =
  OA.option
    r
    $ mconcat
      [ OA.long "commit",
        OA.metavar "HASH",
        mkHelp "Commit hash for which we want to search."
      ]
  where
    r = OA.str >>= FS.OsStr.encodeFail

nameParser :: Parser OsString
nameParser =
  OA.option
    r
    $ mconcat
      [ OA.long "name",
        OA.metavar "STR",
        mkHelpNoLine $
          mconcat
            [ "Repository name. This should be the organization and repo ",
              "following github.com e.g. nixos/nixpkgs for ",
              "github.com/nixos/nixpkgs. Mutually exclusive with --repo."
            ]
      ]
  where
    r = OA.str >>= FS.OsStr.encodeFail

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
      FS.OsStr.decodeLenient versionInfo.gitShortHash,
      ")"
    ]

versLong :: String
versLong =
  L.intercalate
    "\n"
    [ "Git-search: " <> showVersion Paths.version,
      " - Git revision: " <> FS.OsStr.decodeLenient versionInfo.gitHash,
      " - Commit date:  " <> FS.OsStr.decodeLenient versionInfo.gitCommitDate,
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
