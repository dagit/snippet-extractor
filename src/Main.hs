module Main where

import System.Environment ( getArgs )
import System.IO ( withFile, hPutStr, IOMode(..) )
import Data.List ( isInfixOf )
import Data.Char ( isSpace )
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Prim
import Text.Parsec.Combinator

main :: IO ()
main = do
  [file] <- getArgs
  contents <- readFile file
  case runParser parseSnippet () file contents of
    Left  err -> print err
    Right (name, ls) -> do
      withFile name WriteMode $ \h -> do
        hPutStr h ls
  return ()

snippetStart :: String
snippetStart = "@extract-snippet-start"

snippetEnd :: String
snippetEnd = "@extract-snippet-end"

parseName :: Parser FilePath
parseName = do
 manyTill anyChar (try $ string snippetStart)
 spaces
 name <- choice [try $ between (char '"') (char '"')
                               (many (noneOf "\""))
                ,do n <- manyTill anyChar (try newline)
                    return $! takeWhile (not . isSpace) n]
 return name

parseSnippet :: Parser (FilePath, String)
parseSnippet = do
  name <- parseName
  lines <- go []
  return (name, unlines lines)
  where
  go :: [String] -> Parser [String]
  go acc = do
    nextLine <- manyTill anyChar (newline <|> (eof >> return '\n'))
    if snippetEnd `isInfixOf` nextLine
      then return $! reverse acc
      else go (nextLine : acc)

---- Tests
-- TODO: automate these

exampleSnippet1 = 
 "blah\n" ++
 "//@extract-snippet-start foo\n" ++
 "line1 of snippet\n" ++
 "\n" ++
 "\n" ++
 "line4 of snippet\n" ++
 "line5 of snippet\n" ++
 "\n" ++
 "//@extract-snippet-end\n"

exampleSnippet2 = 
 "//@extract-snippet-start foo\n" ++
 "line1 of snippet\n" ++
 "line2 of snippet\n" ++
 "line3 of snippet\n" ++
 "//@extract-snippet-end\n"

exampleSnippet3 = 
 "//@extract-snippet-start foo\n" ++
 "line1 of snippet\n" ++
 "line2 of snippet\n" ++
 "line3 of snippet\n" ++
 "//@extract-snippet-end"

exampleSnippet4 = 
 "//@extract-snippet-start foo\n" ++
 "//@extract-snippet-end"

exampleSnippet5 = 
 "//@extract-snippet-start foo //\n" ++
 "line1 of snippet\n" ++
 "line2 of snippet\n" ++
 "line3 of snippet\n" ++
 "//@extract-snippet-end\n"

parseName_test1 :: Bool
parseName_test1 =
 let line = "//@extract-snippet-start foo \n"
 in case parse parseName "" line of
      Left  _ -> error "failed parseName_test1"
      Right s -> s == "foo"

parseName_test2 :: Bool
parseName_test2 =
 let line = "//@extract-snippet-start \"foo bar\"\n"
 in case parse parseName "" line of
      Left  _ -> error "failed parseName_test2"
      Right s -> s == "foo bar"

parseName_test3 :: Bool
parseName_test3 =
 let line = "/* @extract-snippet-start \"foo bar\" */\n"
 in case parse parseName "" line of
      Left  _ -> error "failed parseName_test2"
      Right s -> s == "foo bar"

parseName_test4 :: Bool
parseName_test4 =
 let line = "/* @extract-snippet-start foo */\n"
 in case parse parseName "" line of
      Left  _ -> error "failed parseName_test2"
      Right s -> s == "foo"
