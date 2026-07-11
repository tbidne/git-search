{-# LANGUAGE CPP #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MagicHash #-}

module Git.Search.Prelude
  ( module X,

    -- * Exceptions
    mapThrowLeft,
    throwLeft,

    -- * Dev
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
import Control.Exception as X
  ( Exception,
    SomeException,
    displayException,
    throwIO,
  )
import Control.Exception.Utils as X (MonadThrow, throwM, throwString, trySync)
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
import Data.Aeson as X (FromJSON (parseJSON), ToJSON)
import Data.Bifunctor as X (first)
import Data.Bool as X (Bool (False, True), not, otherwise, (&&))
import Data.ByteString as X (ByteString)
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
import Data.Ord as X (Ord, (<), (<=), (>))
import Data.Semigroup as X (Semigroup, (<>))
import Data.String as X (IsString, String, fromString)
import Data.Text as X (Text, pack, unpack)
import Data.Text.Encoding.Error as X (UnicodeException)
import Data.Traversable as X (Traversable, for, traverse)
import Data.Tuple as X (fst, snd)
import Data.Word as X (Word32)
import Effectful as X (Eff, IOE, runEff, type (:>))
import Effectful.Concurrent as X (Concurrent, runConcurrent)
import Effectful.Dynamic.Utils as X (showEffectCons)
import Effectful.FileSystem.FileReader.Static as X (FileReader, runFileReader)
import Effectful.FileSystem.HandleReader.Static as X (HandleReader, runHandleReader)
import Effectful.FileSystem.HandleWriter.Dynamic as X (HandleWriter, runHandleWriter)
import Effectful.FileSystem.PathReader.Dynamic as X (PathReader, runPathReader)
import Effectful.FileSystem.PathWriter.Dynamic as X (PathWriter, runPathWriter)
import Effectful.HTTP.Client.Static as X (Network, runNetwork)
import Effectful.Optparse.Static as X (Optparse, osString, runOptparse, validOsPath)
import Effectful.Process as X (Process, runProcess)
import Effectful.Reader.Static as X (Reader, ask, asks, runReader)
import Effectful.Terminal.Dynamic as X
  ( Terminal,
    putStrLn,
    putTextLn,
    runTerminal,
  )
import Effectful.Time.Static as X (Time, runTime)
import FileSystem.OsPath as X
  ( OsPath,
    encodeValidFail,
    encodeValidThrowM,
    osp,
    ospPathSep,
    (</>),
  )
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
    unsafeDecode,
    unsafeEncode,
  )
import FileSystem.UTF8 as X (encodeUtf8)
#if MIN_VERSION_GLASGOW_HASKELL(9, 14, 1, 0)
import FileSystem.Path as X
  ( Abs,
    Dir,
    Path,
    absdir,
    absdirPathSep,
    reldir,
    reldirPathSep,
    toOsPath,
    (<</>>),
    data MkPath
  )
#else
import FileSystem.Path as X
  ( Abs,
    Dir,
    Path,
    absdir,
    absdirPathSep,
    reldir,
    reldirPathSep,
    toOsPath,
    (<</>>),
    pattern MkPath
  )
#endif
import GHC.Err as X (error)
import GHC.Exception (errorCallWithCallStackException)
import GHC.Exts (RuntimeRep, TYPE, raise#)
import GHC.Float as X (Double)
import GHC.Generics as X (Generic)
import GHC.Integer as X (Integer)
import GHC.Num as X (fromInteger, (*), (+), (-))
import GHC.Real as X (floor)
import GHC.Show as X (Show (show))
import GHC.Stack.Types as X (HasCallStack)
import System.Exit as X (ExitCode (ExitFailure, ExitSuccess))
import System.IO as X (IO)
import TOML as X (DecodeTOML (tomlDecoder), getFieldOptWith, getFieldWith)

mapThrowLeft :: (Exception e2, MonadThrow m) => (e1 -> e2) -> Either e1 a -> m a
mapThrowLeft f = throwLeft . first f

throwLeft :: (Exception e, MonadThrow m) => Either e a -> m a
throwLeft (Right x) = pure x
throwLeft (Left e) = throwM e

todo :: forall {r :: RuntimeRep} (a :: TYPE r). (HasCallStack) => a
todo = raise# (errorCallWithCallStackException "Prelude.todo: not yet implemented" ?callStack)
{-# WARNING todo "todo remains in code" #-}
