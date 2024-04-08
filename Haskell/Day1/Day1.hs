module Haskell.Day1.Day1 where

import System.IO ( hGetContents, withFile, IOMode(ReadMode) )
import qualified Data.Map as Map
import Data.Char (isDigit, isAlpha)
import qualified Data.Maybe

parseFile :: FilePath -> IO [String]
parseFile filePath = do
  withFile filePath ReadMode $ \fileHandle -> do
    contents <- hGetContents fileHandle
    let linesOfFile = lines contents
    length linesOfFile `seq` return linesOfFile

findDigit :: String -> Char
findDigit [] = error  "No digit found"
findDigit (x:xs)
  | isDigit x  = x
  | otherwise = findDigit xs

parseLine :: String -> Maybe Int
parseLine [] = Nothing
parseLine xs = stringToInt (findDigit xs : [findDigit (reverse xs)])

wordMap :: Map.Map String Char
wordMap = Map.fromList
  [ ("one", '1')
  , ("two", '2')
  , ("three", '3')
  , ("four", '4')
  , ("five", '5')
  , ("six", '6')
  , ("seven", '7')
  , ("eight", '8')
  , ("nine", '9')
  ]

stringToInt :: String -> Maybe Int
stringToInt str = case reads str of
    [(x,"")] -> Just x
    _        -> Nothing

calculateSum :: Int -> [String] -> Int
calculateSum a [] = a
calculateSum a (x:xs)
  | null x = a
  | otherwise = calculateSum (a + Data.Maybe.fromMaybe 0 (parseLine x)) xs

part1 :: FilePath -> IO ()
part1 filePath = do
  sum <- calculateSum 0 <$> parseFile filePath
  print sum

-----------------------------------------------------

convertLine :: String -> String -> Maybe String
convertLine [] [] = Nothing
convertLine [] y = Just y
convertLine (x:xs) y
  | isDigit x = convertLine (xs) (y ++ [x])
  | isAlpha x = case parseWord (x:xs) [] of
                    Just parsedChar -> convertLine xs (y ++ [parsedChar])
                    Nothing -> convertLine xs y
  | otherwise = Just y

parseWord :: String -> String -> Maybe Char
parseWord [] [] = Nothing
parseWord [] y
  | Map.lookup y wordMap == Nothing = Nothing
  | otherwise = Map.lookup y wordMap
parseWord (x:xs) y
  | isDigit x = Map.lookup y wordMap
  | Map.lookup y wordMap == Nothing = parseWord xs (y ++ [x])
  | otherwise = Map.lookup y wordMap

convertAndCalculateSum :: Int -> [String] -> Int
convertAndCalculateSum a [] = a
convertAndCalculateSum a (x:xs)
  | null x = a
  | otherwise = case convertLine x "" of
                  Just convertedLine -> convertAndCalculateSum (a + Data.Maybe.fromMaybe 0 (parseLine convertedLine)) xs
                  Nothing -> convertAndCalculateSum a xs

part2 :: FilePath -> IO ()
part2 filePath = do
  sum <- convertAndCalculateSum 0 <$> parseFile filePath
  print sum