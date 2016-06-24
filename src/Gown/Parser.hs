-- | Parser for git owners output
module Gown.Parser where

import Text.ParserCombinators.Parsec

ownersData =
  do _ <- string "OWNERS:" <* eol
     owners <- many ownersByFile
     _ <- string "DESCRIPTIONS:" <* eol
     _ <- many (many (noneOf "\n") <* eol)
     return owners

ownersByFile =
  do file <- filePath <* eol
     acls <- many1 acl
     return (file,acls)

acl =
  do aName <- aclName
     users <- names
     _ <- eol
     return (aName,users)

filePath = many1 (char ' ') *> many (noneOf ":\n") <* semi

aclName = many1 (char ' ') *> many (noneOf ":\n") <* semi

name = spaces *> many (noneOf ",\n")

names = sepBy name (char ',')

semi = char ':'

-- eol = string "\n"
eol =
  try (string "\n\r") <|> try (string "\r\n") <|> string "\n" <|>
  string "\r" <?> "end of line"

-- parseOwnersData :: String -> Either ParseError [[String]]
parseOwnersData = parse ownersData "(owners)"

-- parseData :: IO (Either ParseError [[String]])
parseData =
  do ds <- readFile "data/sample-gitowners2.txt"
     let dt = parseOwnersData ds
     return dt