import System.IO ( hGetContents, withFile, IOMode(ReadMode), nativeNewline )
import Data.Char (isDigit, isAlpha)
import Data.Graph (path)
import Data.List (find)

parseFile :: FilePath -> IO [String]
parseFile filePath = withFile filePath ReadMode $ \fileHandle -> do
  contents <- hGetContents fileHandle
  let linesOfFile = lines contents
  length linesOfFile `seq` return linesOfFile

getDirections :: Char -> [(Int, Int)]
getDirections c
    | c == '|' = [(-1,0),(1,0)]
    | c == '-' = [(0,1),(0,-1)]
    | c == 'L' = [(-1,0),(0,1)]
    | c == 'J' = [(-1,0),(0,-1)]
    | c == '7' = [(1,0),(0,-1)]
    | c == 'F' = [(1,0),(0,1)]
    | c == '.' = []
    | c == 'S' = [(-1,0),(1,0),(0,1),(0,-1)]

traceBack :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Bool
traceBack (desRow, desCol) (rowChange, colChange) start = (desRow + rowChange, desCol + colChange) == start

checkPath :: [String] -> [(Int, Int)] -> (Int, Int) -> (Int, Int) -> Maybe (Int, Int)
checkPath grid [] _ _ = Nothing
checkPath grid ((row, column):xs) start lastPosition =
    if (row, column) /= lastPosition
        then
            case find (\change -> traceBack (row, column) change start) (getDirections end) of
                Just change -> Just (row, column)
                Nothing -> checkPath grid xs start lastPosition
        else
            checkPath grid xs start lastPosition
    where
        end = grid !! row !! column


searchViscinity :: [String] -> Char -> (Int, Int) -> (Int, Int) -> Maybe (Int, Int)
searchViscinity grid c (row, col) lastPosition =
    let possible = getDirections c
        destinations = filter (\(r, c) -> r >= 0 && r < length grid && c >= 0 && c < length (head grid))
                              (map (\(rowChange, colChange) -> (rowChange + row, colChange + col)) possible)
    in
        checkPath grid destinations (row, col) lastPosition

startExists :: String -> Int -> Maybe Int
startExists [] _ = Nothing
startExists (x:xs) indx
    | x == 'S' = Just indx
    | otherwise = startExists xs (indx + 1)

findStart :: [String] -> (Int, Int) -> (Int, Int)
findStart [] _ = error "Empty grid"
findStart (line:rest) (row, column) =
    case startExists line 0 of
        Just col -> (row, col)
        Nothing -> findStart rest (row + 1, column)

routePath :: [String] -> (Int, Int) -> (Int, Int) -> (Int, Int) -> Int -> Int
routePath grid beginPoint (startRow, startColumn) lastPos acc =
    case searchViscinity grid char (startRow, startColumn) lastPos of
        Just point | point == beginPoint -> acc
        Just (newRow, newColumn) -> routePath grid beginPoint (newRow, newColumn) (startRow, startColumn) (acc + 1)
        Nothing -> error "Path not found"
    where
        char = grid !! startRow !! startColumn

getMap :: [String] -> (Int, Int) -> (Int, Int) -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
getMap grid beginPoint (startRow, startColumn) lastPos visitedCoords =
    case searchViscinity grid char (startRow, startColumn) lastPos of
        Just point | point == beginPoint -> visitedCoords
        Just (newRow, newColumn) -> getMap grid beginPoint (newRow, newColumn) (startRow, startColumn) ((startRow, startColumn) : visitedCoords)  -- Add the current position to the list of visited coordinates
        Nothing -> error "Path not found"
    where
        char = grid !! startRow !! startColumn

flipSign :: Int -> Int
flipSign n
    | n < 0     = -n
    | otherwise = n

findArea :: [(Int, Int)] -> Int -> Int -> Double
findArea [] n l = fromIntegral (flipSign n `div` 2) + 1 - fromIntegral ( l `div` 2)
findArea [_] n l = fromIntegral (flipSign n `div` 2) + 1 - fromIntegral ( l `div` 2)
findArea pointMap n l =
    let x1 = fst $ head pointMap
        x2 = fst $ head (tail pointMap)
        y1 = snd $ head pointMap
        y2 = snd $ head (tail pointMap)
    in findArea (tail pointMap) (n + (x1 * y2 - x2 * y1)) l

solve :: FilePath -> IO ()
solve filePath = do
    grid <- parseFile filePath
    let start = findStart grid (0, 0)
        result1 = fromIntegral $ routePath grid start start start 0
        modifiedAnswer1 = fromIntegral result1 / 2 + 0.5
        pointMap = getMap grid start start start [start]
        answer2 = findArea (start : pointMap) 0 (length pointMap)
    print $ show modifiedAnswer1 <> " " <> show answer2
