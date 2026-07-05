{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}

module Git.Search.Config
  ( -- * Config
    Config (..),

    -- ** CLI args
    RepoArgs (..),
    Protocol (..),

    -- ** Env
    RepoEnv (..),
    toEnv,

    -- * Aliases
    Args,
    Env,

    -- * Phases
    ConfigPhase (..),
  )
where

import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Effectful (Eff, (:>))
import Effectful.FileSystem.PathReader.Static (PathReader)
import Effectful.FileSystem.PathReader.Static qualified as PR
import Effectful.FileSystem.PathWriter.Static (PathWriter)
import Effectful.FileSystem.PathWriter.Static qualified as PW
import FileSystem.OsString (OsString, osstr)
#if MIN_VERSION_GLASGOW_HASKELL(9, 14, 1, 0)
import FileSystem.Path (Abs, Dir, Path, (<</>>), data MkPath)
#else
import FileSystem.Path (Abs, Dir, Path, (<</>>), pattern MkPath)
#endif
import FileSystem.Path qualified as FS.Path
import GHC.Stack.Types (HasCallStack)

data ConfigPhase
  = ConfigPhaseArgs
  | ConfigPhaseEnv

data Protocol
  = ProtocolHttps
  | ProtocolSsh

data RepoArgs = MkRepoArgs
  { -- | Domain e.g. github.com
    domain :: Maybe OsString,
    -- | Repo name e.g. org/repo
    name :: OsString,
    -- | Protocol e.g. https
    protocol :: Maybe Protocol
  }

data RepoEnv = MkRepoEnv
  { -- | Path to cloned repo e.g. ~/.cache/git-search/org/repo
    path :: Path Abs Dir,
    -- | Full source e.g. https://github.com/org/repo
    src :: OsString
  }

type RepoF :: ConfigPhase -> Type
type family RepoF p where
  RepoF ConfigPhaseArgs = RepoArgs
  RepoF ConfigPhaseEnv = RepoEnv

type Config :: ConfigPhase -> Type
data Config p = MkConfig
  { -- | Performs a clean clone of the repo. Otherwise runs 'fetch' if the
    -- repo exists.
    clean :: Bool,
    -- | Additional debug logging.
    debug :: Bool,
    -- | Hash to search.
    hash :: OsString,
    -- | Repo params.
    repo :: RepoF p
  }

type Args = Config ConfigPhaseArgs

type Env = Config ConfigPhaseEnv

-- | Evolves the CLI Args to runtime Env.
toEnv ::
  ( HasCallStack,
    PathReader :> es,
    PathWriter :> es
  ) =>
  Args ->
  Eff es Env
toEnv args = do
  let protocol = fromMaybe ProtocolHttps args.repo.protocol
      domain = fromMaybe [osstr|github.com|] args.repo.domain
      -- OsString not OsPath since we want slashes preserved.
      prefix = case protocol of
        ProtocolHttps -> [osstr|https://|] <> domain <> [osstr|/|]
        ProtocolSsh -> [osstr|git@|] <> domain <> [osstr|:|]
      src = prefix <> args.repo.name

  -- We get the rootOsP in two steps, rather than the direct
  --
  --   root@(MkPath rootOsP) <- ...
  --
  -- because GHC 9.10 + Effectful incorrectly thinks this is a
  -- failable pattern, hence requires Fail :> es.
  root <- getCacheDir
  let MkPath rootOsP = root
  -- E.g. root := ~/.cache/git-search
  --
  -- Hence, repo := ~/.cache/git-search/org/some-repo
  --
  -- Create cache if it does not exist. The clone step will
  -- take care of creating the repo directory if necessary.
  PW.createDirectoryIfMissing True rootOsP

  pathRel <- FS.Path.parseRelDir args.repo.name
  let path = root <</>> pathRel
      repo =
        MkRepoEnv
          { path,
            src
          }

  pure $
    MkConfig
      { clean = args.clean,
        debug = args.debug,
        hash = args.hash,
        repo
      }

getCacheDir :: (HasCallStack, PathReader :> es) => Eff es (Path Abs Dir)
getCacheDir =
  PR.getXdgCache [osstr|git-search|] >>= FS.Path.parseAbsDir
