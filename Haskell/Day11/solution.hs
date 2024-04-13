import System.IO ( hGetContents, withFile, IOMode(ReadMode), nativeNewline )
import Data.Char (isDigit, isAlpha)
import Data.Graph (path)
import Data.List (find)
import System.Posix.Internals (lstat)

parseFile :: FilePath -> IO [String]
parseFile filePath = withFile filePath ReadMode $ \fileHandle -> do
  contents <- hGetContents fileHandle
  let linesOfFile = lines contents
  length linesOfFile `seq` return linesOfFile



isTupleInList :: Eq a => (a, a) -> [(a, a)] -> Bool
isTupleInList = elem

searchLine :: String -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
searchLine [] (r, c) lst = lst
searchLine (x:xs) (r, c) lst
    | x == '#' = searchLine xs (r, c + 1) ((r,c) : lst)
    | otherwise = searchLine xs (r, c + 1) lst

findGalaxies :: [String] -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
findGalaxies [] (r, c) lst = lst
findGalaxies (row:rows) (r, c) lst = findGalaxies rows (r + 1, c) (searchLine row (r, c) lst)

distance :: (Int, Int) -> (Int, Int) -> Int
distance (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

emptyRows :: [(Int, Int)] -> Int -> Int -> [Int]  -> [Int]
emptyRows points h n res
    | n >= h = res
    | not (any (\(r, c) -> r == n) points) = emptyRows points h (n + 1) (res ++ [n])
    | otherwise = emptyRows points h (n + 1) res

emptyColumns :: [(Int, Int)] -> Int -> Int -> [Int]  -> [Int]
emptyColumns points w n res
    | n >= w = res
    | not (any (\(r, c) -> c == n) points) = emptyRows points w (n + 1) (res ++ [n])
    | otherwise = emptyRows points w (n + 1) res

expandSpace :: [(Int, Int)] -> Int -> [(Int, Int)]
expandSpace points factor =
    let rs = emptyRows points 140 0 []
        cs = emptyColumns points 140 0 []
    in map (\(rowNum, colNum) ->
                let rowNum' = if any (> rowNum) rs then rowNum + factor else rowNum
                    colNum' = if any (> colNum) cs then colNum + factor else colNum
                in (rowNum', colNum')) points

rangeCombinations :: Int -> [(Int, Int)] -> [(Int, Int)]
rangeCombinations d visited =
    let ends = [(-d,0),(d,0),(0,-d),(0,d)]
        inner = d - 1
        rng = [-inner..inner]
    in
    ends ++ filter (\(x, y) -> not ((x == 0 && y == 0) || (x == y)) && notElem (x, y) visited) [(x, y) | x <- rng, y <- rng]

comparePoints :: [(Int, Int)] -> [(Int, Int)] -> (Int, Int)
comparePoints points (pnt:rngPoints)
    | pnt `isTupleInList` points = pnt
    | otherwise = comparePoints points rngPoints

findNearest :: [(Int, Int)] -> [(Int, Int)] -> (Int, Int) -> Int -> (Int, Int) 
findNearest points visited (row, col) rng
    | any (\point -> point `isTupleInList` points) inRange = comparePoints points rangePoints
    | otherwise = findNearest points (rangePoints ++ visited) (row, col) (rng + 1)
    where 
        rangePoints = map (\(r, c) -> (row + r, col + c)) rangeCombinations rng visited

solve :: [String] -> Int
solve grid = searchGalaxies grid [] 0
        where
        points = expandSpace (findGalaxies grid (0,0) []) 1
        searchGalaxies :: [(Int, Int)] -> [(Int, Int)] -> Int -> Int
        searchGalaxies (point:points) visited res