module Main where

import Data.Maybe (isJust, catMaybes, fromJust, isNothing)
import Data.List (transpose)
import Data.List.Split (chunksOf)


---------------- THE BOARD ---------------------

-- Some datatypes
data Symbol = X | O deriving (Show, Eq)
type Field =  Maybe Symbol
type Board = [[Field]]

data GameStatus = Running | Tie | Winner Symbol deriving Show

io = isNothing

showField (Just f) = show f
showField _ = "_"

-- The initial, empty board
initialBoard :: Board
initialBoard = replicate 3 . replicate 3 $ Nothing

-- Places a given symbol at the given position to the given board
-- If the position is already full, it returns Nothing
place :: Board -> Int -> Symbol -> Maybe Board
place board position symbol =
    let arrPos = position - 1
        flatBoard = concat board
        alreadyFilled = isJust $ flatBoard !! arrPos
        (p1,_:p2) = splitAt arrPos flatBoard
        newBoard = chunksOf 3 $ p1 ++ [Just symbol] ++ p2
    in if alreadyFilled then Nothing else Just newBoard


-- Returns a string representation of the Board
printBoard :: Board -> String
printBoard board = unlines $ map ( unwords . map showField ) board


------------------- WINNER-CHECKING ----------------

diagonals :: [[a]] -> [[a]]
diagonals board = [ zipWith (\b i -> b !! i) board [0..] ] ++ [ zipWith (\b i -> b !! i) board [2,1..] ]

checkRow :: [Field] -> Maybe Symbol
checkRow row = 
    let checkSingle (Just x) (Just new) = if x == new then Just x else Nothing
        checkSingle _ _ = Nothing
    in foldl1 checkSingle row

-- Looks if there is a winner
getStatus :: Board -> GameStatus
getStatus board = 
    let checkRows = map checkRow board
        checkColumns = map checkRow $ transpose board
        checkDiags = map checkRow . filter (\d -> length d == 3 ) $ diagonals board
        allResults = catMaybes $ checkRows ++ checkColumns ++ checkDiags
        boardFull = length ( catMaybes $ concat board ) == 9
    in if not (null allResults) then Winner $ head allResults
       else if boardFull then Tie 
            else Running
 

-------------------------- PLAYER COMMUNICATION ------------------


-- Reverses the given symbol
turn :: Symbol -> Symbol
turn (X) = O
turn (O) = X


-- Gets a position from the user. Only returns 1-9
getPosition :: IO Int
getPosition = do
    putStrLn "Which position? [1-9]"
    line <- getLine
    let arr = (reads line :: [(Int,String)])
    if length arr == 0 then do
        putStrLn "Please type in a number!"
        getPosition
    else do
        let pos = fst $ head arr
        if pos >= 1 && pos <= 9 then return pos
        else do
            putStrLn "Please type in a number from 1-9!"
            getPosition


-- Main game loop. 
-- Calls itself recursively with the current board and current symbol as arguments
mainLoop :: Board -> Symbol -> IO ()
mainLoop board symbol = do
    putStrLn $ printBoard board

    pos <- getPosition
    case place board pos symbol of
        Just newboard -> do
            case getStatus newboard of
                Running -> mainLoop newboard $ turn symbol
                Tie -> putStrLn "Tie!"
                Winner x -> putStrLn $ show x ++ " won!"
        Nothing -> do
            putStrLn "This position is full!"
            mainLoop board symbol


-- Runs the main game loop with the initial board and player
main :: IO ()
main = mainLoop initialBoard X

