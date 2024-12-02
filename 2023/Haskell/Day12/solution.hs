import System.IO ( hGetContents, withFile, IOMode(ReadMode), nativeNewline )
import Data.Char (isDigit, isAlpha)
import Data.Graph (path)
import Data.List (find, tails, splitAt, intercalate, foldl')
import Data.Maybe (isJust, fromJust)
import System.Posix.Internals (lstat)
import qualified Data.Map as Map
import qualified Data.Bifunctor

parseFile :: FilePath -> IO [String]
parseFile filePath = withFile filePath ReadMode $ \fileHandle -> do
  contents <- hGetContents fileHandle
  let linesOfFile = lines contents
  length linesOfFile `seq` return linesOfFile

type MemoKey = (String, [Int])
type MemoCache = Map.Map MemoKey Int

updateMemo :: MemoCache -> MemoKey -> Int -> (Int, MemoCache)
updateMemo cache key val = (val, Map.insert key val cache)

calculateLine :: MemoCache -> MemoKey -> Int -> (Int, MemoCache)
calculateLine cache key@(str, groups) acc
    | isJust res = (fromJust res, cache)
    where 
        res = Map.lookup key cache
calculateLine cache key@([], groups) acc = (acc, cache)
calculateLine cache key@(str, []) acc
    | '#' `notElem` str = updateMemo cache key 1
    | otherwise = updateMemo cache key 0
calculateLine cache key@(str, groups) acc
    | length str < (sum groups + length groups - 1) = updateMemo cache key 0
    | c == '#' = updateMemo cache'' key (acc + val')
    | c == '.' = updateMemo cache' (rest, groups) (acc + val)
    | c == '?' = updateMemo cache'' key (acc + val + val')
    | otherwise = (acc, cache)
    where
        (c:rest) = str
        (val, cache') = calculateLine cache (rest, groups) acc
        (val', cache'') = pound cache (str, groups)


pound :: MemoCache -> MemoKey -> (Int, MemoCache)
pound  cache key@(str, g:groups) =
    let group = take g str
        allNums = replaceChar '?' '#' group
    in
        if allNums /= replicate g '#' then updateMemo cache key 0
        else if length str == g then
            if length (g:groups) == 1 then updateMemo cache key 1
            else updateMemo cache key 0
        else
            if str !! g `elem` "?." then
                if null groups then 
                    updateMemo cache' key val
                else 
                    updateMemo cache'' key val'
            else updateMemo cache key 0
            where (val, cache') = calculateLine cache (drop g (tail str), groups) 1
                  (val', cache'') = calculateLine cache (drop g (tail str), groups) 0

updateCache :: String -> [Int] -> Int -> MemoCache -> MemoCache
updateCache str nums res cache =
    case Map.lookup (str, nums) cache of
        Just _ -> Map.insert (str, nums) res cache
        Nothing -> Map.insert (str, nums) res cache

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
multiplyLine (str, nums) =
    let newStr = str ++ "?" ++ str ++ "?" ++ str ++ "?" ++ str ++ "?" ++ str
        newLst = nums ++ nums ++ nums ++ nums ++ nums
        in (newStr, newLst)

part1 :: [String] -> Int -> MemoCache -> Int
part1 xs res cache = foldl' (\ res x ->  res + fst (calculateLine cache (parseLine x) 0)) res xs

part2 :: [String] -> Int -> MemoCache -> Int
part2 xs res cache = foldl' (\ res x -> res + fst (calculateLine cache (multiplyLine $ parseLine x) 0)) res xs

solve :: FilePath -> IO ()
solve filePath = do
    grid <- parseFile filePath
    let answer1 = part1 grid 0 Map.empty
    let answer2 = part2 grid 0 Map.empty
    print $ show answer1 ++ " " ++ show answer2

main :: IO ()
main = solve "input.txt"
