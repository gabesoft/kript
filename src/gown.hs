-- | Show git owners for a particular commit
{-# LANGUAGE ExtendedDefaultRules, OverloadedStrings #-}

import Control.Monad (forM_, forM)
import qualified Data.Text as T
import Shelly
import System.Environment (getArgs)
import Gown.Parser

default (T.Text)

main =
  shelly $
  verbosely $
  do args <- liftIO getArgs
     case args of
       [sha] ->
         do input <-
              escaping False $
              cmd "git"
                  (T.pack $ "show --pretty='format:' --name-status " ++ sha)
            files <-
              forM (T.lines input) $
              \line ->
                escaping False $ cmd "ls" (T.append "-lA " $ T.tail line)
            owners <- escaping False $ cmd "cat data/sample-gitowners.txt"
            echo input
            echo $ T.concat files
            echo owners
       _ -> echo "Usage: gitowners sha"
     echo (T.pack . show $ args)
     echo "hello from shell"

-- | Parse input text and generate an association list containing entries
-- | of the form (file, user)
parseOwners text = undefined

-- | Given an association list with entries of the form (file, user)
-- | compute the smallest group of users that covers all files or return
-- | Nothing if not possible
getGroups assoc = undefined
