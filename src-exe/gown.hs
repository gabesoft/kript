-- | Show git owners for a particular commit
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad (forM, forM_)
import qualified Data.Text as T
import           Gown.Parser
import           Gown.Processor
import           Shelly
import           System.Environment (getArgs)
import qualified Text.Show.Pretty as Pretty

default (T.Text)

-- usage: cat file | gown
main =
  shelly $
  do args <- liftIO getArgs
     text <- liftIO getContents
     let ast = unpack $ parseAcls text
     let groups = findAclGroups ast
     let aclsByOwner = aclTypesByOwner ast
     -- echo "Original AST:"
     -- echo $ T.pack $ (Pretty.ppShow ast)
     echo "Acl types by owner:"
     echo $ T.pack $ (Pretty.ppShow aclsByOwner)
     echo "Best groups:"
     echo $ T.pack $ (Pretty.ppShow groups)

unpack :: Either a [b] -> [b]
unpack (Left _) = []
unpack (Right xs) = xs

-- main =
--   shelly $
--   verbosely $
--   do args <- liftIO getArgs
--      case args of
--        [sha] ->
--          do input <-
--               escaping False $
--               cmd "git"
--                   (T.pack $ "show --pretty='format:' --name-status " ++ sha)
--             files <-
--               forM (T.lines input) $
--               \line ->
--                 escaping False $ cmd "ls" (T.append "-lA " $ T.tail line)
--             owners <- escaping False $ cmd "cat data/sample-gitowners.txt"
--             echo input
--             echo $ T.concat files
--             echo owners
--        _ -> echo "Usage: gitowners sha"
--      echo (T.pack . show $ args)
--      echo "hello from shell"

