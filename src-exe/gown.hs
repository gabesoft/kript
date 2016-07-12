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
     putStrLn $ heading1 "Acl types by owner:"
     mapM_ printOwnerAcl aclsByOwner
     putStrLn ""
     putStrLn $ heading1 "Best groups:"
     mapM_ printGroup groups

printOwnerAcl :: (String,[String]) -> IO ()
printOwnerAcl (owner,acls') =
  do putStrLn owner
     mapM_ putAcl acls'
  where putAcl acl' = putStrLn $ black "  - " ++ acl'

printGroup :: [String] -> IO ()
printGroup group' = do putStrLn str
  where str = join $ intersperse "," group'

heading1 = yellow

heading2 = blue

unpack :: Either a [b] -> [b]
unpack = either (const []) id

black :: String -> String
black = chalk "black"

red :: String -> String
red = chalk "red"

green :: String -> String
green = chalk "green"

yellow :: String -> String
yellow = chalk "yellow"

blue :: String -> String
blue = chalk "blue"

magenta :: String -> String
magenta = chalk "magenta"

cyan :: String -> String
cyan = chalk "cyan"

white :: String -> String
white = chalk "white"

gray :: String -> String
gray = chalk "gray"

mkEsc :: Int -> String
mkEsc value = "\x1b[" ++ show value ++ "m"

chalk :: String -> String -> String
chalk color str = (mkEsc start) ++ str ++ (mkEsc end)
  where (start,end) =
          maybe (39,39)
                id
                (lookup color colors)

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