module Main (main) where

import Integration.DeleteCache qualified
import Integration.Prelude
import Integration.SearchBranch qualified
import Integration.SearchCommit qualified
import Integration.SearchDefaults qualified
import Integration.SearchPullRequest qualified

main :: IO ()
main =
  defaultMain
    $ testGroup
      "Integration"
      [ Integration.DeleteCache.tests,
        Integration.SearchBranch.tests,
        Integration.SearchCommit.tests,
        Integration.SearchDefaults.tests,
        Integration.SearchPullRequest.tests
      ]
