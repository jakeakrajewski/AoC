import Text.Parsec
import Text.Parsec.String (Parser)
import System.IO ( hGetContents, withFile, IOMode(ReadMode) )
import qualified Data.Map as Map
import Data.Char (isDigit, isAlpha)
import qualified Data.Maybe
import Data.Either (fromRight)

data GameData = GameData Int [[(String, Int)]] deriving Show

redMax :: Int
redMax = 12

greenMax :: Int
greenMax = 13

blueMax :: Int
blueMax = 14

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
    return (GameData gameNum dataSets)

validateDataSet :: [(String, Int)] -> Bool
validateDataSet [] = True
validateDataSet ((color, count):xs)
    | color == "red" && count > redMax = False
    | color == "green" && count > greenMax = False
    | color == "blue" && count > blueMax = False
    | otherwise = validateDataSet xs

validateGame :: GameData -> Maybe Int
validateGame (GameData gameNum datasets) =
    if all validateDataSet datasets
        then Just gameNum
        else Nothing

calculateSum :: [String] -> Int -> Int
calculateSum [] n = n
calculateSum (x:xs) n =
    let gameResult = parse parseGame "" x
    in case gameResult of
        Left err -> error $ "Parse error on " ++ show err
        Right game -> calculateSum xs (n + Data.Maybe.fromMaybe 0 (validateGame game))

part1 :: FilePath -> IO ()
part1 filePath = do
    games <- parseFile filePath
    let result = calculateSum games 0
    print result

------------------------------------------------

getMins :: [(String, Int)] -> Int -> Int -> Int -> Int
getMins [] r g b = r * g * b
getMins ((color, count):xs) r g b
    | color == "red" && count > r = getMins xs count g b
    | color == "green" && count > g = getMins xs r count b
    | color == "blue" && count > b = getMins xs r g count
    | otherwise = r * g * b

aggregatePower :: [[(String, Int)]]-> Int -> Maybe Int
aggregatePower [] n = Nothing
aggregatePower (x:xs) n =
    let result = getMins x 0 0 0
    in
        aggregatePower xs (n + result)

calculatePower :: [String] -> Int -> Int
calculatePower [] n = n
calculatePower (x:xs) n =
    let gameResult = parse parseGame "" x
        gameData = fromRight (error "failed to parse") gameResult
        datasets = case gameData of
                     GameData _ ds -> ds
    in
        calculatePower xs (n + Data.Maybe.fromMaybe 0 (aggregatePower datasets 1))


part2 :: FilePath -> IO ()
part2 filePath = do
    games <- parseFile filePath
    let result = calculatePower games 0
    print result

