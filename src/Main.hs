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

main :: IO ()
main = putStrLn "Hello, Haskell!"
