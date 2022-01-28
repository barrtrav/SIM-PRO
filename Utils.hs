module Utils (
    (%%),
    getDir,
    newStd,
    randomInt,
    Board(..),
    printBoard,
    createBoard,
    randomLimInt,
    getStringLine,
    validPosition,
    checkAdjacent,
) where

import Items
import Data.Array
import System.Random

type Board = Array (Int, Int) Item

(%%) :: Int -> Int -> Int
(%%) x a = div (x*a) 100

createBoard :: Int -> Int -> Board
createBoard n m = array ((0,0), (n, m)) ([((i,j), Item False False False False False False) | i <- range(0,n), j <- range(0,m)])

getStringLine :: Int -> String
getStringLine n = if n == 1 then "------" else "------" ++ x where x = getStringLine $ n - 1

printBoard :: Board -> Int -> Int -> String
printBoard board n m = unlines [ "-" ++ getStringLine n ++ "\n| " ++ unwords [ show (board ! (i, j)) ++ " |" | i <- [0..n-1]] | j <- [0..m-1]] ++ "-" ++ getStringLine n 

randomInt :: Int -> StdGen -> Int
randomInt m rand = mod (fst x) m where x = next rand

randomLimInt :: Int -> Int -> StdGen -> Int
randomLimInt mn mx rand = mn + (mod (fst x) (mx - mn)) where x = next rand

newStd :: StdGen -> StdGen
newStd rng = snd $ next rng

validPosition :: Int -> Int -> Int -> Int -> Bool
validPosition n m x y = (x >= 0 && x < n && y >= 0 && y < m)

checkAdjacent :: Board -> Int -> Int -> Int -> Int -> Bool
checkAdjacent board n m x y = 
    if (validPosition n m (x+1) y && (getItem (board ! (x+1, y)) "Y")) ||
       (validPosition n m (x-1) y && (getItem (board ! (x-1, y)) "Y")) ||
       (validPosition n m x (y+1) && (getItem (board ! (x, y+1)) "Y")) ||
       (validPosition n m x (y-1) && (getItem (board ! (x, y-1)) "Y")) then True
    else False

getDir :: (Int, Int) -> Int -> (Int,Int)
getDir (i,j) index 
    | index == 0 = (i-1,j)
    | index == 1 = (i,j-1)
    | index == 2 = (i+1,j)
    | index == 3 = (i,j+1)
    | otherwise = (i,j)
