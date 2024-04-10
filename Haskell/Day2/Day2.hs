import Text.Parsec
import Text.Parsec.String (Parser)
import System.IO ( hGetContents, withFile, IOMode(ReadMode) )
import qualified Data.Map as Map
import Data.Char (isDigit, isAlpha)
import qualified Data.Maybe
import Data.Either (fromRight)

data GameData = GameData Int [(String, Int)] deriving Show

parseFile :: FilePath -> IO [String]
parseFile filePath = do
  withFile filePath ReadMode $ \fileHandle -> do
    contents <- hGetContents fileHandle
    let linesOfFile = lines contents
    length linesOfFile `seq` return linesOfFile

parseInt :: Parser Int
parseInt = read <$> many1 digit

parseColor :: Parser String
parseColor = choice [string "blue", string "red", string "green"]

parseDataSet :: Parser [(String, Int)]
parseDataSet = sepBy parseDataPoint (char ',' >> spaces)

parseDataPoint :: Parser (String, Int)
parseDataPoint = do
    count <- parseInt
    spaces
    color <- parseColor
    return (color, count)

parseGame :: Parser GameData
parseGame = do
    string "Game "
    gameNum <- parseInt
    char ':'
    spaces
    dataSets <- sepBy parseDataSet (char ';' >> spaces)
    return (GameData gameNum (concat dataSets))

validateDataSet :: (String, Int) -> Bool
validateDataSet ([], i) = True
validateDataSet (color, count)
    | color == "red" && count > 12 = False
    | color == "green" && count > 13 = False
    | color == "blue" && count > 14 = False
    | otherwise = True

validateGame :: GameData -> Maybe Int
validateGame (GameData gameNum datasets) =
    if all validateDataSet datasets
        then Just gameNum
        else Nothing

calculateSum :: [String] -> Int -> Int
calculateSum (x:xs) n = foldl (\ n x -> n + Data.Maybe.fromMaybe 0 (validateGame (fromRight (error "error") $ parse parseGame "" x))) n xs

part1 :: FilePath -> IO ()
part1 filePath = do
    games <- parseFile filePath
    let result = calculateSum games 0
    print result

------------------------------------------------

getMins :: [(String, Int)] -> (Int, Int, Int) -> Int
getMins [] (r, g, b) = r * g * b
getMins ((color, count):xs) (r, g, b)
    | color == "red" && count > r = getMins xs (count, g, b)
    | color == "green" && count > g = getMins xs (r, count, b)
    | color == "blue" && count > b = getMins xs (r, g, count)
    | otherwise = getMins xs (r, g, b)

calculatePower :: [String] -> Int -> Int
calculatePower xs n = foldl (\ n x -> getMins (case fromRight (error "failed to parse") $ parse parseGame "" x of GameData _ ds -> ds) (0, 0, 0) + n) n xs

part2 :: FilePath -> IO ()
part2 filePath = do
    games <- parseFile filePath
    let result = calculatePower games 0
    print result

