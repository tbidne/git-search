{-# LANGUAGE CPP #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MagicHash #-}

module Git.Search.Prelude
  ( module X,
    todo,
  )
where

import Control.Applicative as X
  ( Applicative,
    liftA2,
    liftA3,
    pure,
    (<*>),
    (<|>),
  )
import Control.Category as X ((>>>))
import Control.Exception as X (Exception, displayException)
import Control.Exception.Utils as X (throwString, trySync, throwM)
import Control.Monad as X
  ( Monad,
    MonadFail,
    fail,
    forever,
    unless,
    when,
    (<=<),
    (=<<),
    (>=>),
    (>>=),
  )
import Control.Monad.IO.Class as X (MonadIO, liftIO)
import Data.Bifunctor as X (first)
import Data.Bool as X (Bool (False, True), not, otherwise, (&&))
import Data.Either as X (Either (Left, Right), either)
import Data.Eq as X (Eq, (/=), (==))
import Data.Foldable as X (Foldable, for_, length, traverse_)
import Data.Function as X (const, flip, id, ($), (.))
import Data.Functor as X (Functor (fmap), void, (<$>), (<&>))
import Data.Int as X (Int)
import Data.Kind as X (Type)
import Data.List as X (List, (++))
import Data.List.NonEmpty as X (NonEmpty ((:|)))
import Data.Map.Strict as X (Map)
import Data.Maybe as X (Maybe (Just, Nothing), fromMaybe, maybe)
import Data.Monoid as X (Monoid, mconcat, mempty)
import Data.Ord as X (Ord, (<), (>))
import Data.Semigroup as X (Semigroup, (<>))
import Data.String as X (IsString, String, fromString)
import Data.Text as X (Text, pack, unpack)
import Data.Traversable as X (Traversable, for, traverse)
import Data.Tuple as X (fst, snd)
import Effectful as X (Eff, IOE, runEff, type (:>))
import Effectful.Concurrent as X (Concurrent, runConcurrent)
import Effectful.FileSystem.FileReader.Static as X (FileReader, runFileReader)
import Effectful.FileSystem.HandleReader.Static as X (HandleReader, runHandleReader)
import Effectful.FileSystem.HandleWriter.Static as X (HandleWriter, runHandleWriter)
import Effectful.FileSystem.PathReader.Static as X (PathReader, runPathReader)
import Effectful.FileSystem.PathWriter.Static as X (PathWriter, runPathWriter)
import Effectful.Optparse.Static as X (Optparse, runOptparse, osString)
import Effectful.Process as X (Process, runProcess)
import Effectful.Terminal.Dynamic as X
  ( Terminal,
    putStrLn,
    putTextLn,
    runTerminal,
  )
import Effectful.Time.Static as X (Time, runTime)
import FileSystem.OsPath as X (OsPath, encodeValidFail, osp, ospPathSep, (</>))
import FileSystem.OsString as X
  ( OsString,
    decode,
    decodeLenient,
    decodeThrowM,
    encode,
    encodeFail,
    encodeLenient,
    encodeThrowM,
    osstr,
  )
#if MIN_VERSION_GLASGOW_HASKELL(9, 14, 1, 0)
import FileSystem.Path as X (Abs, Dir, Path, toOsPath, (<</>>), data MkPath)
#else
import FileSystem.Path as X (Abs, Dir, Path, toOsPath, (<</>>), pattern MkPath)
#endif
import GHC.Err as X (error)
import GHC.Exception (errorCallWithCallStackException)
import GHC.Exts (RuntimeRep, TYPE, raise#)
import GHC.Integer as X (Integer)
import GHC.Num as X (fromInteger, (*), (+), (-))
import GHC.Real as X (floor)
import GHC.Show as X (Show (show))
import GHC.Stack.Types as X (HasCallStack)
import System.Exit as X (ExitCode (ExitFailure, ExitSuccess))
import System.IO as X (IO)
import TOML as X (DecodeTOML (tomlDecoder))

todo :: forall {r :: RuntimeRep} (a :: TYPE r). (HasCallStack) => a
todo = raise# (errorCallWithCallStackException "Prelude.todo: not yet implemented" ?callStack)
{-# WARNING todo "todo remains in code" #-}
