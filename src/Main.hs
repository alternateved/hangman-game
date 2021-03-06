module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.List (intersect, intersperse)
import Data.Maybe (isJust)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

newtype WordList = WordList [String] deriving (Eq, Show)

-- Generate a list of words from file
allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ WordList (lines dict)

-- Set words length limit
minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

-- Filter allWords accordingly to provided word's length constraint
gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allWords
  return $ WordList (filter gameLength aw)
  where
    gameLength w =
      let l = length (w :: String)
       in l >= minWordLength && l < maxWordLength

-- Generate random number and return a word from WordList with index of random number
randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIndex <- randomRIO (0, length wl - 1)
  return $ wl !! randomIndex

-- Get random word from gameWords by binding gameWords to randomWord
randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle = Puzzle String [Maybe Char] [Char]

puzzleWord :: Puzzle -> String
puzzleWord (Puzzle s _ _) = s

-- Show Puzzle's word to guess (String), characters filled in ([Maybe Char]), letters guessed ([Char])
instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    fmap renderPuzzleChar discovered
      ++ " Guessed so far: "
      ++ intersperse ' ' guessed

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

-- Calculate number of wrong guesses
countWrongs :: Puzzle -> Int
countWrongs (Puzzle wordToGuess _ guessed) = length guessed - rightChars
  where
    rightChars = length $ intersect wordToGuess guessed

-- Terminate game if the number of wrong guesses is over the limit
gameOver :: Puzzle -> IO ()
gameOver puzzle =
  if countWrongs puzzle > limit
    then do
      putStrLn "You lose!"
      putStrLn $ "The word was: " ++ puzzleWord puzzle
      exitSuccess
    else return ()
  where
    limit = 5

-- Win the game if the word is guessed
gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) =
  if all isJust filledInSoFar
    then do
      putStrLn "You win!"
      exitSuccess
    else return ()

-- Run the game forever! (until the word is guessed or the limit of wrong guesses breached)
runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $ "\nCurrent puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _ -> putStrLn "Your guess must be a single character"

main :: IO ()
main = do
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle
