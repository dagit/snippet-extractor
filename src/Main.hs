module Main where

import System.Environment ( getArgs )
import System.IO ( withFile, hPutStr, IOMode(..) )
import Data.List ( isInfixOf )
import Data.Char ( isSpace )
import Data.Maybe ( catMaybes )
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Prim
import Text.Parsec.Combinator

main :: IO ()
main = do
  args <- getArgs
  if "--help" `elem` args
    then printUsage
    else mapM_ processFile args
 where
 processFile file = do
   contents <- readFile file
   case runParser parseSnippets () file contents of
     Left  err -> print err
     Right snippets -> mapM_ (\(name,ls) -> do
       withFile name WriteMode $ \h -> do
       hPutStr h ls) snippets
 printUsage = do
   putStr $ "Usage: snippet-extractor [FILE1 FILE2 ...]\n" ++
            "\n" ++
            "The snippet-extractor looks for snippets and puts them in\n"++
            "their own files.  This can make it easier to include snippets\n"++
            "in LaTeX or DocBook files.\n"++
            "\n" ++
            "A snippet has the following form:\n"++
            "  /* @snippet-start snippet1.c */\n"++
            "  for(i = 0; i < 100; i++){\n"++
            "    printf(\"%d\\n\",i);\n"++
            "  }\n"++
            "  /* @snippet-end */\n"++
            "\n" ++
            "The above snippet would be extracted and placed in snippet1.c\n"++
            "\n" ++
            "Note: If the filename you want the snippet place in has spaces,\n"++
            "      then put the name in quotes like this:\n"++
            "      @snippet-start \"destination file.c\"\n"

snippetStart :: String
snippetStart = "@snippet-start"

snippetEnd :: String
snippetEnd = "@snippet-end"

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

parseSnippets :: Parser [(FilePath, String)]
parseSnippets = many (try parseSnippet)

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
