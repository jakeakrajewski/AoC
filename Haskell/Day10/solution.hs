import System.IO ( hGetContents, withFile, IOMode(ReadMode) )
import Data.Char (isDigit, isAlpha)
import Data.Graph (path)
import Data.List (find)

parseFile :: FilePath -> IO [String]
parseFile filePath = do
  withFile filePath ReadMode $ \fileHandle -> do
    contents <- hGetContents fileHandle
    let linesOfFile = lines contents
    length linesOfFile `seq` return linesOfFile

getDirections :: Char -> [(Int, Int)]
getDirections c
    | c == '|' = [(-1,0),(1,0)]
    | c == '-' = [(0,-1),(0,-1)]
    | c == 'L' = [(-1,0),(0,1)]
    | c == 'J' = [(-1,0),(0,-1)]
    | c == '7' = [(1,0),(0,-1)]
    | c == 'F' = [(1,0),(0,1)]
    | c == '.' = []
    | c == 'S' = [(-1,0),(1,0),(0,1),(0,-1)]

traceBack :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Bool
traceBack (desRow, desCol) (rowChange, colChange) start = (desRow - rowChange, desCol - colChange) == start

checkPath :: [String] -> [(Int, Int)] -> (Int, Int) -> (Int, Int) -> Maybe (Int, Int)
checkPath grid [] _ _ = Nothing
checkPath grid ((row, column):xs) start lastPosition =
    case find (\change -> traceBack (row, column) change start) (getDirections end) of
        Just change -> Just change
        Nothing -> checkPath grid xs start lastPosition
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


solve :: FilePath -> IO ()
solve filePath = do
    contents <- readFile filePath
    let grid = lines contents
        start = findStart grid (0, 0)
        answer1 = routePath grid start start start 0
    print $ show answer1 
    -- <> " " <> show answer2