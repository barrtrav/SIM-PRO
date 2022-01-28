module Main where

import Data.Array
import System.Random
import System.Environment

import Items
import Utils
import Simulations


nArgs :: [String] -> Int -> Int
nArgs args i =
    if (i+1) < (length args) then
        if (args !! i) == "-n" then read $ args !! (i+1)
        else nArgs args (i+2)
    else 2 

bArgs :: [String] -> Int -> Int
bArgs args i =
    if (i+1) < (length args) then
        if (args !! i) == "-b" then read $ args !! (i+1)
        else bArgs args (i+2)
    else 3

rArgs :: [String] -> Int -> Int
rArgs args i =
    if (i+1) < (length args) then
        if (args !! i) == "-r" then read $ args !! (i+1)
        else mArgs args (i+2)
    else 3 

oArgs :: [String] -> Int -> Int
oArgs args i =
    if (i+1) < (length args) then
        if (args !! i) == "-o" then read $ args !! (i+1)
        else mArgs args (i+2)
    else 3 

mArgs :: [String] -> Int -> Int
mArgs args i =
    if (i+1) < (length args) then
        if (args !! i) == "-m" then read $ args !! (i+1)
        else mArgs args (i+2)
    else 2  

main :: IO()
main = do
    
    putStrLn ""
    putStrLn "#####################################################"
    putStrLn "#                                                   #"
    putStrLn "#         Welcome to Robot-House simulation         #"
    putStrLn "#                                                   #"
    putStrLn "#####################################################"
    putStrLn ""

    args <- getArgs

    let m = mArgs args 0  -- Squares Row
    let n = nArgs args 0  -- Squares Columns

    m <- return(m * 3)
    n <- return(n * 3)

    putStrLn ("[+] Creating board of " ++ (show m) ++ "x" ++ (show n))

    let c_boy = bArgs args 0 -- Number of boy on board
    let c_robot = rArgs args 0  -- Number of robot on board
    let c_object = oArgs args 0  -- Number of object on board

    if (n*m) < ((c_boy * 2) + c_object + c_robot) then do
        putStrLn "[-] Error in number of item (don't have capcity)..."
        putStrLn "[-] End simulation..."
        return ()
    else do

    rand <- newStdGen

    let board = createBoard n m

    putStrLn "[+] Generating items..."

    (board, rand) <- return(generateYards board n m c_boy c_boy rand)
    (board, rand) <- return(generateBoy board n m c_boy rand)
    (board, rand) <- return(generateRobot board n m c_robot rand)
    (board, rand) <- return(generateObject board n m c_object rand)

    -- Generate a random int between 50% and 90% for dirty items
    let c_dirty = randomLimInt (((n*m) - (c_boy * 2 + c_object + c_robot)) %% 40) (((n*m) - (c_boy * 2 + c_object + c_robot)) %% 90) (newStd rand)
    putStrLn "[+] Generating initial dirty..."
    (board, rand) <- return(generateDirty board n m c_dirty rand)

    putStrLn "[+] Initial state for:"
    putStrLn ("\t [*] Number of boy: " ++ (show c_boy))
    putStrLn ("\t [*] Number of robot: " ++ (show c_robot))
    putStrLn ("\t [*] Number of object: " ++ (show c_object))
    
    putStrLn "\n Time: 0 \n"
    putStrLn $ (printBoard board n m ++ "\n")

    simulation board n m 1

    putStrLn "[+] End simulation..."

    return()

generateYards :: Board -> Int -> Int -> Int -> Int -> StdGen -> (Board, StdGen)
generateYards board n m count total rand =
    if count == total && count > 0 then
        generateYards (board // [((x,y), Item False True False False False False)]) n m (count-1) total (newStd $ newStd rand)
    else if count > 0 then
        if (isEmpty $ board ! (x,y)) && (checkAdjacent board n m x y) then
            generateYards (board // [((x,y), Item False True False False False False)]) n m (count-1) total (newStd $ newStd rand)
        else generateYards board n m count total (newStd $ newStd rand)
    else (board, rand)
    where
        x = randomInt n rand
        y = randomInt m (newStd rand) 

generateBoy :: Board -> Int -> Int -> Int -> StdGen -> (Board, StdGen)
generateBoy board n m count rand =
    if count > 0 then
        if isEmpty $ board ! (x,y) then
            generateBoy (board // [((x,y), Item False False True False False False)]) n m (count-1) (newStd $ newStd rand)
        else generateBoy board n m count (newStd $ newStd rand)
    else (board, rand)
    where
        x = randomInt n rand
        y = randomInt m (newStd rand)

generateRobot :: Board -> Int -> Int -> Int -> StdGen -> (Board, StdGen)
generateRobot board n m count rand =
    if count > 0 then
        if isEmpty $ board ! (x,y) then
            generateRobot (board // [((x,y), Item False False False True False False)]) n m (count-1) (newStd $ newStd rand)
        else generateRobot board n m count (newStd $ newStd rand)
    else (board, rand)
    where
        x = randomInt n rand
        y = randomInt m (newStd rand)

generateObject :: Board -> Int -> Int -> Int -> StdGen -> (Board, StdGen)
generateObject board n m count rand =
    if count > 0 then
        if isEmpty $ board ! (x,y) then
            generateObject (board // [((x,y), Item False False False False True False)]) n m (count-1) (newStd $ newStd rand)
        else generateObject board n m count (newStd $ newStd rand)
    else (board, rand)
    where
        x = randomInt n rand
        y = randomInt m (newStd rand)

generateDirty :: Board -> Int -> Int -> Int -> StdGen -> (Board, StdGen)
generateDirty board n m count rand =
    if count > 0 then
        if isEmpty $ board ! (x,y) then
            generateDirty (board // [((x,y), Item True False False False False False)]) n m (count-1) (newStd $ newStd rand)
        else generateDirty board n m count (newStd $ newStd rand)
    else (board, rand)
    where
        x = randomInt n rand
        y = randomInt m (newStd rand)