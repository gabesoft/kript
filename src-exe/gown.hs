-- | Compute a minimal list of owners that own all files in a commit
-- | Usage: git show --pretty='format:' --name-only <sha> | xargs git owners | gown
import qualified Data.Text as T
import Control.Monad
import Data.List
import Data.Maybe (maybe)
import Gown.Parser
import Gown.Processor
import System.Environment (getArgs)
import System.Process

default (T.Text)

main :: IO ()
main =
  do args <- getArgs
     text <- getContents
     let ast = unpack $ parseAcls text
     let groups = findAclGroups 20 ast -- TODO make this an argument
     let aclsByOwner = aclTypesByOwner ast
     putStrLn $ heading "Acl types by owner:"
     mapM_ printOwnerAcl aclsByOwner
     putStrLn $ heading "Best groups:"
     mapM_ printGroup groups

printOwnerAcl :: (String,[String]) -> IO ()
printOwnerAcl (owner,acls') =
  do putStrLn $ owner ++ ":"
     mapM_ putAcl acls'
  where putAcl acl' = putStrLn $ "  - " ++ acl'

printGroup :: [String] -> IO ()
printGroup group' = do putStrLn str
  where str = join $ intersperse "," group'

heading str = chalk "yellow" $ "\n\n" ++ str ++ "\n"

unpack :: Either a [b] -> [b]
unpack = either (const []) id

chalk :: String -> String -> String
chalk color str = (colorEsc start) ++ str ++ (colorEsc end)
  where (start,end) =
          maybe (39,39)
                id
                (lookup color colors)
        colorEsc value = "\x1b[" ++ show value ++ "m"

colors :: [(String,(Int,Int))]
colors =
  [("black",(30,39))
  ,("red",(31,39))
  ,("green",(32,39))
  ,("yellow",(33,39))
  ,("blue",(34,39))
  ,("magenta",(35,39))
  ,("cyan",(36,39))
  ,("white",(37,39))
  ,("gray",(90,39))]
