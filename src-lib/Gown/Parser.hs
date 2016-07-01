-- | Parser for git owners output
module Gown.Parser
       (AclEntry(..), AclOwners(..), acls, fileOwners, acl, filePath,
        aclName, names, name, parseAcls, parseAclsSample)
       where

import Text.ParserCombinators.Parsec
import Text.Parsec.Char (endOfLine)
import Control.Applicative ((*>), (<*), (<*>), (<$>))

data AclEntry =
  AclEntry {aclFile :: FilePath
           ,aclOwners :: [AclOwners]}
  deriving (Eq,Ord,Show,Read)

data AclOwners =
  AclOwners {aclType :: String
            ,aclNames :: [String]}
  deriving (Eq,Ord,Show,Read)

-- | Parses the owners section from the output of a git owners command
acls :: CharParser () [AclEntry]
acls = (string "OWNERS:" <* eol) *> many1 fileOwners

-- | Parses one entry of the owners section from the output of a git owners command
-- | It returns the file path and the associated acl owners
fileOwners :: CharParser () AclEntry
fileOwners = AclEntry <$> (filePath <* eol) <*> (many1 $ try $ acl <* end)

-- | Parses an acl type and the owners belonging to this type
acl :: CharParser () AclOwners
acl = AclOwners <$> aclName <*> names

filePath :: CharParser () FilePath
filePath =
  spaces1 *> many (nameExcluded ":" "a valid file path character") <*
  semi <?> "file path"

aclName :: CharParser () String
aclName =
  spaces1 *> many (nameExcluded ":" "a valid acl name character") <*
  semi <?> "acl name"

spaces1 :: CharParser () ()
spaces1 = space *> spaces

name :: CharParser () String
name = spaces *> many (nameExcluded " :" "a valid name character") <?> "name"

names :: CharParser () [String]
names = name `sepBy` (char ',') <?> "multiple names"

semi :: CharParser () Char
semi = char ':'

eol :: CharParser () Char
eol = endOfLine

end :: CharParser () ()
end = (eol >> return ()) <|> eof

nameExcluded
  :: String -> String -> CharParser () Char
nameExcluded chars desc = (noneOf $ ",\t\v\f\r\n" ++ chars) <?> desc

parseAcls
  :: String -> Either ParseError [AclEntry]
parseAcls = parse acls "(owners)"

parseAclsSample
  :: IO (Either ParseError [AclEntry])
parseAclsSample =
  do ds <- readFile "data/sample-gitowners2.txt"
     let dt = parseAcls ds
     return dt