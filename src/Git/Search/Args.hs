module Git.Search.Args
  ( Args (..),
    getArgs,
  )
where

import FileSystem.OsString (OsString)
import FileSystem.OsString qualified as FS.OsStr
import FileSystem.Path (Dir, Path, Rel)
import FileSystem.Path qualified as FS.Path
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

data Args = MkArgs
  { cache :: Bool,
    debug :: Bool,
    hash :: OsString,
    repoName :: Path Rel Dir
  }

getArgs :: IO Args
getArgs = OA.execParser parserInfoArgs
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
      cache <- cacheParser
      debug <- debugParser
      hash <- hashParser
      repoName <- repoNameParser

      pure $
        MkArgs
          { cache,
            debug,
            hash,
            repoName
          }

cacheParser :: Parser Bool
cacheParser =
  OA.switch $
    mconcat
      [ OA.long "cache",
        mkHelp $
          mconcat
            [ "Caches the repository. Useful when cloning is expensive, and ",
              "we want to search for multiple hashes."
            ]
      ]

debugParser :: Parser Bool
debugParser =
  OA.switch $
    mconcat
      [ OA.long "debug",
        mkHelp "Enables additional logging."
      ]

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

repoNameParser :: Parser (Path Rel Dir)
repoNameParser =
  OA.option
    r
    $ mconcat
      [ OA.long "repo",
        OA.metavar "REPO",
        mkHelp $
          mconcat
            [ "Repository name. This should be the organization and repo ",
              "following github.com e.g. nixos/nixpkgs for ",
              "github.com/nixos/nixpkgs."
            ]
      ]
  where
    r = do
      nameStr <- OA.str
      nameOsStr <- FS.OsStr.encodeFail nameStr

      case FS.Path.parseRelDir nameOsStr of
        Nothing ->
          fail $
            mconcat
              [ "Failed parsing relative directory from: ",
                nameStr
              ]
        Just name -> pure name

mkHelp :: String -> Mod f a
mkHelp =
  OA.helpDoc
    . fmap (<> Pretty.hardline)
    . Chunk.unChunk
    . Chunk.paragraph
