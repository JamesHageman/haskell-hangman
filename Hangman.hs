module Hangman where

import System.Environment (getArgs)
import Data.Set as Set
import Data.String.Utils (join)
import Data.List.Split


data Hangman = Hangman
  { word :: String
  , guesses :: Set Char
  , orderedGuesses :: [Char]
  }


data GameStatus = Playing | Won | Lost deriving (Eq)


maxBadGuesses = 5


gameStatus :: Hangman -> GameStatus
gameStatus (Hangman {word = word, guesses = guesses})
  | correctGuesses `isSubsetOf` guesses = Won
  | size badGuesses >= maxBadGuesses    = Lost
  | otherwise                           = Playing
  where
    correctGuesses = Set.fromList word
    badGuesses = guesses `difference` correctGuesses


init :: String -> Hangman
init word = Hangman word empty []


view :: Hangman -> String
view state =
  maskedWord state ++ "\nGuesses: " ++ (formattedGuesses state) ++ "\n> "


formattedGuesses :: Hangman -> String
formattedGuesses (Hangman {orderedGuesses = orderedGuesses}) =
  join " " $ splitOn "" $ reverse orderedGuesses


maskedWord :: Hangman -> String
maskedWord (Hangman {word = word, guesses = guesses}) =
  fmap (guessChar guesses) word


guessChar :: Set Char -> Char -> Char
guessChar guesses c
  | c `member` guesses = c
  | otherwise = '_'


update :: String -> Hangman -> Hangman
update input state
  | length input == 0 = state
  | otherwise = applyGuess guess state
  where guess = head input


applyGuess :: Char -> Hangman -> Hangman
applyGuess guess (Hangman {word = word, guesses = guesses, orderedGuesses = orderedGuesses}) =
  Hangman
    { word = word
    , guesses = insert guess guesses
    , orderedGuesses = guess : orderedGuesses
    }


isGameOver :: Hangman -> Bool
isGameOver state =
  gameStatus state /= Playing


runGame :: Hangman -> (Hangman -> String) -> (String -> Hangman -> Hangman) -> IO ()
runGame model view update = do
  putStr (view model)
  if isGameOver model
    then return ()
    else do
      input <- getLine
      let newModel = update input model
      runGame newModel view update
