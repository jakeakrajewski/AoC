import Text.Parsec
import Text.Parsec.String (Parser)
import System.IO ( hGetContents, withFile, IOMode(ReadMode) )
import qualified Data.Map as Map
import Data.Char (isDigit, isAlpha)
import qualified Data.Maybe
import Data.Either (fromRight)
import Data.List (sort, group)


parseFile :: FilePath -> IO [String]
parseFile filePath = do
  withFile filePath ReadMode $ \fileHandle -> do
    contents <- hGetContents fileHandle
    let linesOfFile = lines contents
    length linesOfFile `seq` return linesOfFile

checkSymbol :: Char -> Bool
checkSymbol c = c `notElem` "1234567890."

checkLineForChars :: String -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
checkLineForChars [] _ lst = lst
checkLineForChars str (l, n) lst
    | checkSymbol $ head str = checkLineForChars (tail str) (l, n + 1) ((l, n):lst)
    | otherwise = checkLineForChars (tail str) (l, n + 1) lst

rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort

checkPoint :: [String] -> Int -> Int -> Int -> Int -> Int -> Int
checkPoint lst rowOffset colOffset row col sum
    | 0 <= rowPoint && rowPoint < 140 &&
      0 <= colPoint && colPoint < 140 &&
        isDigit char =
            findNumber (lst !! rowPoint) colPoint ""
    | otherwise = 0
    where
        rowPoint = row + rowOffset
        colPoint = col + colOffset
        char = (lst !! rowPoint) !! colPoint

searchAdjacent :: [String] -> (Int, Int) -> [Int]
searchAdjacent lst (row, col) =
    rmdups (filter (/= 0)
        [checkPoint lst rowOffset colOffset row col 0 |
            rowOffset <- [-1..1], colOffset <- [-1..1]])

findNumber :: String -> Int -> String -> Int
findNumber line ind str = read ( isNumberBackward line ind "" ++ [line !! ind] ++  isNumberForward line ind "" ) :: Int

isNumberForward :: String -> Int -> String -> String
isNumberForward str ind result
    | ind + 1 < 140 && isDigit (str !! (ind + 1)) = isNumberForward str (ind + 1) (result ++ [str !! (ind + 1)])
    | otherwise = result

isNumberBackward :: String -> Int -> String -> String
isNumberBackward str ind result
    | ind - 1 >= 0 && isDigit (str !! (ind - 1)) = isNumberBackward str (ind - 1) ((str !! (ind - 1)) : result)
    | otherwise = result

findSymbols :: [String] -> [(Int, Int)]
findSymbols strings = foldl accumulateSymbols [] (zip strings [0..])
    where
        accumulateSymbols :: [(Int, Int)] -> (String, Int) -> [(Int, Int)]
        accumulateSymbols acc (str, lineNum) =
            acc ++ checkLineForChars str (lineNum, 0) []

aggregateSumPart1 :: [String] -> Int
aggregateSumPart1 lst = sum allInts
    where
    symbols = findSymbols lst
    allInts = concatMap (searchAdjacent lst) symbols


aggregateSumPart2 :: [String] -> Int
aggregateSumPart2 lst = sum allInts
    where
        symbols = findSymbols lst
        validSymbols = filter (\symbol -> length (searchAdjacent lst symbol) == 2) symbols
        allInts = map (product . searchAdjacent lst) validSymbols

solve :: FilePath -> IO ()
solve filePath = do
    contents <- readFile filePath
    let linesOfFile = lines contents
        sum1 = aggregateSumPart1 linesOfFile
        sum2 = aggregateSumPart2 linesOfFile
    print $ show sum1 <> " " <> show sum2

