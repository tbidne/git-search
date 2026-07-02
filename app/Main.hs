module Main (main) where

import Git.Search qualified
import Git.Search.Args qualified
import System.IO qualified as IO

main :: IO ()
main = do
  -- Needed in case another command runs this and tries to read the output.
  IO.hSetBuffering IO.stderr IO.LineBuffering
  IO.hSetBuffering IO.stdout IO.LineBuffering

  Git.Search.Args.getArgs >>= Git.Search.search
