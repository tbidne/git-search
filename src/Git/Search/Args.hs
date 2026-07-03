module Git.Search.Args
  ( getArgs,
  )
where

import Effectful (Eff, (:>))
import Effectful.Optparse.Static (Optparse)
import Effectful.Optparse.Static qualified as EOA
import FileSystem.OsString (OsString)
import FileSystem.OsString qualified as FS.OsStr
import Git.Search.Config
  ( Args,
    Config (MkConfig, clean, debug, hash, repo),
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
import Options.Applicative.Help.Chunk (Chunk (Chunk))
import Options.Applicative.Help.Chunk qualified as Chunk
import Options.Applicative.Help.Pretty qualified as Pretty
import Options.Applicative.Types (ArgPolicy (Intersperse))

getArgs :: (Optparse :> es) => Eff es Args
getArgs = EOA.execParser parserInfoArgs
  where
    parserInfoArgs =
      ParserInfo
        { infoParser = argsParser,
          infoFullDesc = True,
          infoProgDesc = desc,
          infoHeader = Chunk headerTxt,
          infoFooter = Chunk Nothing,
          infoFailureCode = 1,
          infoPolicy = Intersperse
        }
    headerTxt = Just "git-search: Searches a git repository for commit hashes."

    desc =
      Chunk.paragraph $
        mconcat
          [ "Given a git commit hash, returns a list of branches containing ",
            "that hash."
          ]

argsParser :: Parser Args
argsParser = do
  p <**> OA.helper
  where
    p = do
      ~(hash, name) <- parseRequired

      ~(domain, protocol) <- parseRepo

      ~(clean, debug) <- parseMisc

      pure $
        MkConfig
          { clean,
            debug,
            hash,
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
          <$> hashParser
          <*> nameParser

    parseRepo =
      OA.parserOptionGroup "Repository options:" $
        (,)
          <$> domainParser
          <*> protocolParser

    parseMisc =
      OA.parserOptionGroup "Miscellaneous options:" $
        (,)
          <$> cleanParser
          <*> debugParser

cleanParser :: Parser Bool
cleanParser =
  OA.switch $
    mconcat
      [ OA.long "clean",
        mkHelp $
          mconcat
            [ "Performs a clean clone of the repo, overwriting any previous ",
              "repo."
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

hashParser :: Parser OsString
hashParser =
  OA.option
    r
    $ mconcat
      [ OA.long "hash",
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
