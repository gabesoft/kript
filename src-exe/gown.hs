{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Compute a minimal list of owners that own all files in a commit
-- | Usage: git show --pretty='format:' --name-only <sha> | xargs git owners | gown
import qualified Data.Text as T
import Gown.Parser
import Gown.Processor
import Shelly
import System.Environment (getArgs)
import qualified Text.Show.Pretty as Pretty

default (T.Text)

main :: IO ()
main =
  shelly $
  do args <- liftIO getArgs
     text <- liftIO getContents
     let ast = unpack $ parseAcls text
     let groups = findAclGroups 50 ast -- TODO make this an argument
     let aclsByOwner = aclTypesByOwner ast
     -- echo "Original AST:"
     -- echo $ T.pack $ (Pretty.ppShow ast)
     echo "Acl types by owner:"
     echo $ T.pack $ (Pretty.ppShow aclsByOwner)
     echo "Best groups:"
     echo $ T.pack $ (Pretty.ppShow groups)

unpack :: Either a [b] -> [b]
unpack = either (const []) id
