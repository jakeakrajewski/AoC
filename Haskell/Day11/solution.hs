import System.IO ( hGetContents, withFile, IOMode(ReadMode), nativeNewline )
import Data.Char (isDigit, isAlpha)
import Data.Graph (path)
import Data.List (find, tails)
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

emptyRows :: [String] -> Int -> Int -> [Int]  -> [Int]
emptyRows grid h n res
    | n >= h = res
    | '#' `notElem` (grid !! n) = emptyRows grid h (n + 1) (res ++ [n])
    | otherwise = emptyRows grid h (n + 1) res

emptyColumns :: [String] -> Int -> Int -> [Int]  -> [Int]
emptyColumns grid w n res
    | n >= w = res
    | not (any (\r -> r !! n == '#') grid) = emptyColumns grid w (n + 1) (res ++ [n])
    | otherwise = emptyColumns grid w (n + 1) res

expandSpace :: [String] -> [(Int, Int)] -> Int -> [(Int, Int)]
expandSpace grid points factor =
    let rs = emptyRows grid 140 0 []
        cs = emptyColumns grid 140 0 []
    in map (\(rowNum, colNum) ->
                let rowNum' = rowNum + countGreater rs rowNum * factor
                    colNum' = colNum + countGreater cs colNum * factor
                in (rowNum', colNum')) points

countGreater :: [Int] -> Int -> Int
countGreater list value = length $ filter (< value) list

comparePoints :: [(Int, Int)] -> [(Int, Int)] -> (Int, Int)
comparePoints points (pnt:rngPoints)
    | pnt `isTupleInList` points = pnt
    | otherwise = comparePoints points rngPoints

part1 :: [String] -> Int
part1 grid = sum [distance p1 p2 | (p1:rest) <- tails points, p2 <- rest]
    where
        points = expandSpace grid (findGalaxies grid (0,0) []) 1

part2 :: [String] -> Int
part2 grid = sum [distance p1 p2 | (p1:rest) <- tails points, p2 <- rest]
    where
        points = expandSpace grid (findGalaxies grid (0,0) []) 999999

solve :: FilePath -> IO ()
solve filePath = do
    grid <- parseFile filePath
    let answer1 = part1 grid
    let answer2 = part2 grid
    print $ show answer1 <> " " <> show answer2