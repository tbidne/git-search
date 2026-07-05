module Main (main) where

import Effectful qualified
import Effectful.FileSystem.PathReader.Static qualified as PR
import Effectful.FileSystem.PathWriter.Static qualified as PW
import Effectful.Optparse.Static qualified as EOA
import Effectful.Process qualified as P
import Effectful.Terminal.Dynamic qualified as Term
import Effectful.Time.Static qualified as Time
import Git.Search qualified
import Git.Search.Args qualified
import System.IO qualified as IO

main :: IO ()
main = do
  -- Needed in case another command runs this and tries to read the output.
  IO.hSetBuffering IO.stderr IO.LineBuffering
  IO.hSetBuffering IO.stdout IO.LineBuffering

  Effectful.runEff
    . PR.runPathReader
    . PW.runPathWriter
    . EOA.runOptparse
    . P.runProcess
    . Term.runTerminal
    . Time.runTime
    $ Git.Search.searchPrint
      =<< Git.Search.Args.getArgs
