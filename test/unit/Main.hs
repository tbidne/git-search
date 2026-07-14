module Main (main) where

import Unit.Git.Search.Config qualified
import Unit.Git.Search.Config.Args qualified
import Unit.Prelude

main :: IO ()
main =
  defaultMain
    $ testGroup
      "Unit"
      [ Unit.Git.Search.Config.tests,
        Unit.Git.Search.Config.Args.tests
      ]
