-- | Parser for git owners output
module Gown.Parser where

import Text.ParserCombinators.Parsec

data AclEntry =
  AclEntry {aclFile :: FilePath
           ,aclOwners :: [AclOwners]}
  deriving(Eq, Show, Read)

data AclOwners =
  AclOwners {aclType :: String
            ,aclNames :: [String]}
  deriving(Eq, Show, Read)

acls :: CharParser () [AclEntry]
acls =
  do _ <- string "OWNERS:" <* eol
     owners <- many fileOwners
     _ <- string "DESCRIPTIONS:" <* eol
     _ <- many (many (noneOf "\n") <* eol)
     return owners

fileOwners :: CharParser () AclEntry
fileOwners =
  do file <- filePath <* eol
     fileAcls <- many1 (try acl)
     return $ AclEntry file fileAcls

acl :: CharParser () AclOwners
acl =
  do aName <- aclName <* spaces
     users <- names <* eol
     return $ AclOwners aName users

filePath :: CharParser () FilePath
filePath =
  many1 (char ' ') *> many (nameExcluded ":" "a valid file path character") <*
  semi <?> "file path"

aclName :: CharParser () String
aclName =
  many1 (char ' ') *> many (nameExcluded ":" "a valid acl name character") <*
  semi <?> "acl name"

name = spaces *> many (nameExcluded " :" "a valid name character") <?> "name"

names = sepBy name (char ',') <?> "multiple names"

semi = char ':'

eol =
  try (string "\n\r") <|> try (string "\r\n") <|> string "\n" <|>
  string "\r" <?> "end of line"

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