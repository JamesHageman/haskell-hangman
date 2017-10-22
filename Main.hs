module Main where

import Hangman
import System.Environment

main :: IO ()
main = do
  word <- fmap head $ getArgs
  runGame (Hangman.init word) view update
