module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.List (intersperse)
import Data.Maybe (isJust)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

type WordList = [String]

-- Generate a list of words from file
allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return (lines dict)

-- Set words length limit
minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

-- Filter allWords accordingly to provided word's length constraint
gameWords :: IO WordList
gameWords = do filter gameLength <$> allWords
  where
    gameLength w =
      let l = length (w :: String)
       in l >= minWordLength && l < maxWordLength

-- Generate random number and return a word from WordList with index of random number
randomWord :: WordList -> IO String
randomWord wl = do
  randomIndex <- randomRIO (0, length wl - 1)
  return $ wl !! randomIndex

-- Get random word from gameWords by binding gameWords to randomWord
randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle = Puzzle String [Maybe Char] [Char]

-- Show Puzzle's word to guess (String), characters filled in ([Maybe Char]), letters guessed ([Char])
instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    intersperse ' ' $
      fmap renderPuzzleChar discovered
        ++ " Guessed so far: "
        ++ guessed

-- Create a list of Nothing from Puzzle
freshPuzzle :: String -> Puzzle
freshPuzzle s = Puzzle s chars []
  where
    chars = map (const Nothing) s

-- Check if character is an element of the Puzzle String
charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle s _ _) c = c `elem` s

-- Check if character is an element of the Puzzle's letters guessed list
alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ l) c = c `elem` l

-- Render Puzzle character - '_' if not guessed, Char if guessed
renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just c) = c

-- Insert correctly guessed character into the string
fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar l) c = Puzzle word newFilledInSoFar (c : l)
  where
    zipper guessed wordChar guessChar =
      if wordChar == guessed
        then Just wordChar
        else guessChar
    newFilledInSoFar = zipWith (zipper c) word filledInSoFar

-- Handle interaction with player
handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn "You already guessed that character, pick something else!"
      return puzzle
    (True, _) -> do
      putStrLn "This character was in the word, filling in the word accordingly"
      return (fillInCharacter puzzle guess)
    (False, _) -> do
      putStrLn "This character wasn't in the word, try again."
      return (fillInCharacter puzzle guess)

main :: IO ()
main = putStrLn "Hello, Haskell!"
