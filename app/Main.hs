module Main where

import           Control.Exception
import           Data.List
import           Safe
import           System.Directory
import           System.FilePath.Posix
import           System.Random
import           System.Random.Shuffle
import           WithCli

import           Parse
import           Utils

main :: IO ()
main = withCli run

run :: FilePath -> IO ()
run sourceDir = do
  book <- randomBook sourceDir
  case book of
    Nothing -> abort "no book found."
    Just book -> do
      rawText <- readFile $ sourceDir </> book
      let verses = consumeText "" [] rawText
      verse <- randomVerse verses
      putStrLn verse

consumeText :: String -> [String] -> String -> [String]
consumeText b acc s = do
  case parseVerse s of
    Left _ -> acc
    Right (Verse book chapter verseNumber verse, rest) -> do
      consumeText bookName acc' rest
      where
        bookName = case book of
          Nothing -> b
          Just b' -> b'
        acc' = (verse ++ "\n" ++ "- " ++ wipe bookName ++ " (" ++ chapter ++ ":" ++ verseNumber ++ ")\n") : acc

shuffleList :: [a] -> IO [a]
shuffleList x = do
  g <- getStdGen
  let shuffled = shuffle' x (length x) g
  return shuffled

randomBook :: FilePath -> IO (Maybe FilePath)
randomBook sourceDir = do
  books <- getDirectoryContents sourceDir
  let validBooks = filter (".txt" `isSuffixOf`) books
  if length validBooks == 0
    then do
      abort "invalid source specified."
    else do
      shuffledBooks <- shuffleList validBooks
      let mBook = headMay shuffledBooks
      return $ case mBook of
        Nothing -> Nothing
        Just book -> Just book

randomVerse :: [String] -> IO String
randomVerse verses = do
  shuffledVerses <- shuffleList verses
  let mVerse = headMay shuffledVerses
  return $ case mVerse of
    Nothing -> ""
    Just verse -> verse
