module Utils where

import           Data.Char

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

wipe :: String -> String
wipe s
  = let
      repl '\r' = ' '
      repl c = c
    in map repl s
