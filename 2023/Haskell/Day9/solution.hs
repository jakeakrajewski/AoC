import System.IO ( hGetContents, withFile, IOMode(ReadMode) )
import Data.Char (isDigit, isAlpha)

parseFile :: FilePath -> IO [String]
parseFile filePath = do
  withFile filePath ReadMode $ \fileHandle -> do
    contents <- hGetContents fileHandle
    let linesOfFile = lines contents
    length linesOfFile `seq` return linesOfFile

stringToIntList :: String -> [Int]
stringToIntList str = map toInt (words str)
  where
    toInt :: String -> Int
    toInt s
      | all isInt (tail s) = read s
      | otherwise = error $ "Invalid number: " ++ s
    isInt :: Char -> Bool
    isInt c = isDigit c || c == '-'

findDiffs :: [Int] -> [Int] -> [Int]
findDiffs [] res = res
findDiffs [x] res = res
findDiffs (x:xs) res = findDiffs xs (res ++  [head xs - x])

drillDown :: [Int] -> [[Int]] -> [[Int]]
drillDown [] res = res
drillDown lst res
    | any (/= 0) lst = drillDown (findDiffs lst []) (lst:res)
    | otherwise = lst : res

nextIteration :: [[Int]] -> Int -> Int
nextIteration [] diff = 0
nextIteration [x] diff = last x + diff
nextIteration (x:xs) diff = nextIteration xs (last x + diff )

prevIteration :: [[Int]] -> Int -> Int
prevIteration [] diff = 0
prevIteration [x] diff = head x - diff
prevIteration (x:xs) diff = prevIteration xs (head x - diff )

solve :: FilePath -> IO ()
solve filePath = do
    contents <- readFile filePath
    let linesOfFile = map stringToIntList $ lines contents
        answer1 = sum $ foldl (\acc line -> nextIteration (drillDown  line []) 0 : acc) [] linesOfFile
        answer2 = sum $ foldl (\acc line -> prevIteration (drillDown line []) 0 : acc) [] linesOfFile
    print $ show answer1 <> " " <> show answer2