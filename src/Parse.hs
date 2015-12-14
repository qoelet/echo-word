module Parse (
  Verse(..)
, parseVerse
) where

import           Control.Monad (void)
import           Text.Parsec ((<?>), (<|>))
import qualified Text.Parsec as P
import           Text.Parsec.Char (char, satisfy)
import           Text.Parsec.Combinator (many1, manyTill)
import           Text.Parsec.String (Parser)

import           Utils

data BibleText
  = VerseInfo String String
  | VerseContent String
  | BookName String
  deriving (Eq, Show)

data Verse = Verse (Maybe String) String String String
  deriving (Eq, Show)

-- parsers

verseInfo :: Parser BibleText
verseInfo = do
  void $ lexeme $ char '{'
  e <- lexeme $ many1 verseLoc
  void $ char '}'
  return (VerseInfo (takeWhile (/= ':') e) ((tail . dropWhile (/= ':')) e))
  where
    verseLoc = satisfy (`elem` ":0123456789")

verse :: Parser BibleText
verse = do
  v <- lexeme $ P.try (manyTill P.anyChar (char '{')) <|> many1 P.anyToken
  return $ VerseContent v

bookName :: Parser BibleText
bookName = do
  whitespace
  P.notFollowedBy (char '{')
  v <- lexeme $ P.try (manyTill P.anyChar (char '\n')) <|> many1 P.anyToken
  return $ BookName v

whitespace :: Parser ()
whitespace = void $ P.many $ P.oneOf " \r\n\t"

lexeme :: Parser a -> Parser a
lexeme p = do
           x <- p
           whitespace
           return x

parse :: Parser a -> String -> Either P.ParseError (a, String)
parse p = P.parse ((,) <$> wrapper <*> leftOver) ""
  where
    wrapper = whitespace >> p
    leftOver = P.manyTill P.anyToken P.eof

parseVerse :: String -> Either P.ParseError (Verse, String)
parseVerse s = do
    (VerseInfo c v, rest) <- parse verseInfo s'
    (VerseContent contents, rest') <- parse verse rest
    let contents' = (trim . filter (/= '\n') . wipe) contents
    return (Verse bookName c v contents', replaceCurly rest')
  where
    (bookName, s') = parseBookName s

    replaceCurly :: String -> String
    replaceCurly s
      | not (null s) = "{" ++ s
      | otherwise = s

parseBookName :: String -> (Maybe String, String)
parseBookName s
  = case parse bookName s of
    Left _ -> (Nothing, s)
    Right (BookName name, rest) -> (Just name, rest)
