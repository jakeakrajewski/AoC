import System.IO ( hGetContents, withFile, IOMode(ReadMode), nativeNewline )
import Data.Char (isDigit, isAlpha)
import Data.Graph (path)
import Data.List (find, tails, splitAt, intercalate)
import System.Posix.Internals (lstat)

parseFile :: FilePath -> IO [String]
parseFile filePath = withFile filePath ReadMode $ \fileHandle -> do
  contents <- hGetContents fileHandle
  let linesOfFile = lines contents
  length linesOfFile `seq` return linesOfFile

calculateLine :: String -> [Int] -> Int -> Int
calculateLine [] groups res = res
calculateLine str [] res
    | '#' `notElem` str = 1
    | otherwise = 0
calculateLine (c:cs) (n:ns) res
    | c == '#' = calculateLine cs (n:ns) (res + pound (c:cs) (n:ns))
    | c == '.' = calculateLine cs (n:ns) res
    | c == '?' = (calculateLine cs (n:ns) res) + (pound (c:cs) (n:ns))
    | otherwise = res

pound :: String -> [Int] -> Int
pound (c:cs) (n:ns) =
    let group = take n (c:cs)
        allNums = replaceChar '?' '#' group
    in
        if allNums /= replicate n '#' then 0
        else if length (c:cs) == n then
            if length ns == 1 then 1
            else 0
        else
            if (c:cs) !! n `elem` "?." then calculateLine (drop n cs) ns 0
            else 0

parseLine :: String -> (String, [Int])
parseLine str =
    let (left, right) = break (== ' ') str
        ints =stringToIntList right
    in (left, ints)

replaceChar :: Char -> Char -> String -> String
replaceChar oldChar newChar = map (\c -> if c == oldChar then newChar else c)

stringToIntList :: String -> [Int]
stringToIntList str = map read (split ',' str)
  where split :: Eq a => a -> [a] -> [[a]]
        split _ [] = []
        split delim str = first : split delim rest
          where (first, rest) = break (== delim) (dropWhile (== delim) str)

part1 :: [String] -> Int -> Int
part1 xs res = foldl (\ res x -> res + uncurry calculateLine (parseLine x) 0) res xs

part2 :: [String] -> Int -> Int
part2 [] res = res

solve :: FilePath -> IO ()
solve filePath = do
    grid <- parseFile filePath
    let answer1 = part1 grid 0
    -- let answer2 = part2 grid
    print $ show answer1