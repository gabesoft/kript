-- | Show git owners for a particular commit
{-# LANGUAGE ExtendedDefaultRules, OverloadedStrings #-}

import Control.Monad (forM_, forM)
import qualified Data.Text as T
import Shelly
import System.Environment (getArgs)

default (T.Text)

main =
  shelly $
  verbosely $
  do args <- liftIO getArgs
     case args of
       [sha] ->
         do input <- escaping False $ cmd "git" "show --pretty='format:' --name-status HEAD"
            echo input
       _ -> echo "Usage: gitowners sha"
     echo (T.pack . show $ args)
     echo "hello from shell"