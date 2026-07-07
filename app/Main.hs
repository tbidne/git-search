module Main (main) where

import Git.Search.Network (runNetwork)
import Git.Search.Prelude hiding (IO)
import Git.Search.Runner qualified
import System.IO (IO)

main :: IO ()
main =
  runEff
    . runConcurrent
    . runFileReader
    . runHandleReader
    . runHandleWriter
    . runNetwork
    . runPathReader
    . runPathWriter
    . runOptparse
    . runProcess
    . runTerminal
    . runTime
    $ Git.Search.Runner.runSearch
