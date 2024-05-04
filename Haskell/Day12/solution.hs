import System.IO ( hGetContents, withFile, IOMode(ReadMode), nativeNewline )
import Data.Char (isDigit, isAlpha)
import Data.Graph (path)
import Data.List (find, tails, splitAt, intercalate)
import System.Posix.Internals (lstat)
import qualified Data.Map as Map

parseFile :: FilePath -> IO [String]
parseFile filePath = withFile filePath ReadMode $ \fileHandle -> do
  contents <- hGetContents fileHandle
  let linesOfFile = lines contents
  length linesOfFile `seq` return linesOfFile

type MemoKey = (String, [Int])

type MemoCache = Map.Map MemoKey Int

memoizedCalculateLine :: String -> [Int] -> Int -> MemoCache -> (Int, MemoCache)
memoizedCalculateLine str nums acc cache =
    case Map.lookup (str, nums) cache of
        Just result -> (result, cache)
        Nothing -> let (result, newCache) = calculateLine str nums acc memoizedCalculateLine cache
                    in (result, Map.insert (str, nums) result newCache)

calculateLine :: String -> [Int] -> Int -> (String -> [Int] -> Int ->  MemoCache -> (Int, MemoCache)) -> MemoCache -> (Int, MemoCache)
calculateLine [] groups res memoFunc cache = (res, cache)
calculateLine str [] res memoFunc cache
    | '#' `notElem` str = (1, cache)
    | otherwise = (0, cache)
calculateLine (c:cs) (n:ns) res memoFunc cache
    | c == '#' = pound (c:cs) (n:ns) cache
    | c == '.' = memoizedCalculateLine cs (n:ns) res cache
    | c == '?' = (fst (memoizedCalculateLine cs (n:ns) res cache) + fst (pound (c:cs) (n:ns) cache), cache)
    | otherwise = (res, cache)

pound :: String -> [Int] -> MemoCache -> (Int, MemoCache)
pound (c:cs) (n:ns) cache =
    let group = take n (c:cs)
        allNums = replaceChar '?' '#' group
    in
        if allNums /= replicate n '#' then (0, cache)
        else if length (c:cs) == n then
            if length (n:ns) == 1 then (1, cache)
            else (0, cache)
        else
            if (c:cs) !! n `elem` "?." then do
                if null ns then memoizedCalculateLine (drop n cs) ns 1 cache
                else memoizedCalculateLine (drop n cs) ns 0 cache
            else (0, cache)

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

multiplyLine :: (String, [Int]) -> (String, [Int])
multiplyLine (str, nums) = (concat (replicate 5 str), concat (replicate 5 nums))

emptyMemoCache :: MemoCache
emptyMemoCache = Map.empty

part1 :: [String] -> Int -> MemoCache -> Int
part1 xs res cache = foldl (\ res x ->  res + fst (uncurry memoizedCalculateLine (parseLine x) 0 cache)) res xs

part2 :: [String] -> Int -> MemoCache -> Int
part2 xs res cache = foldl (\ res x -> res + fst (uncurry memoizedCalculateLine (multiplyLine $ parseLine x) 0 cache)) res xs

solve :: FilePath -> IO ()
solve filePath = do
    grid <- parseFile filePath
    let answer1 = part1 grid 0 Map.empty
    -- let answer2 = part2 grid 0 Map.empty
    print $ show answer1 -- ++ " " ++ show answer2

main :: IO ()
main = solve "input.txt"
