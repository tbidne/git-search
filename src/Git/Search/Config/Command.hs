module Git.Search.Config.Command
  ( Command (..),
  )
where

import Git.Search.Prelude

newtype Command
  = SearchCommit OsString
