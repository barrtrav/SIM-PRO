module Simulations (
    simulation
) where

import Items
import Utils

import Data.Array
import System.Random

finishSimulation :: Board -> Int -> Int -> Bool
finishSimulation board n m = 
    clean >= ((clean + dirty) %% 60)
    where
        clean = countCleanItems board n m (0,0) 0
        dirty = countDirtyItems board n m (0,0) 0

countCleanItems :: Board -> Int -> Int -> (Int, Int) -> Int -> Int
countCleanItems board n m (i,j) count =
    if i < n then 
        if j < m then
            if isEmpty $ board ! (i,j) then
                countCleanItems board n m (i,j+1) (count+1)
            else countCleanItems board n m (i,j+1) count
        else countCleanItems board n m (i+1,0) count
    else count

countDirtyItems :: Board -> Int -> Int -> (Int, Int) -> Int -> Int
countDirtyItems board n m (i,j) count =
    if i < n then 
        if j < m then
            if (isOnlyDirty $ board ! (i,j)) || (isOnlyDirty1 $ board ! (i,j)) || (isOnlyDirty2 $ board ! (i,j)) then
                countDirtyItems board n m (i,j+1) (count+1)
            else countDirtyItems board n m (i,j+1) count
        else countDirtyItems board n m (i+1,0) count
    else count

simulation :: Board -> Int -> Int -> Int -> IO()
simulation board n m time = do

    if finishSimulation board n m then do
        putStrLn "[p] 60% of board is clean..."
        return ()
    else do

    rand <- newStdGen
    (newBoard, string1) <- return(stepBoy board (createBoard n m) n m 0 0 rand "")
    rand <- return(newStd $ newStd $ newStd rand)
    (newBoard, string2) <- return(stepRobot newBoard (createBoard n m) n m 0 0 rand "")
    (newBoard, string) <- return(generateDirty board newBoard n m 0 0 rand "")

    putStrLn ("Time " ++ (show time) ++ "\n")
    putStrLn (string1 ++ string2)
    putStrLn ((printBoard newBoard n m) ++ "\n")

    simulation newBoard n m (time+1)

stepBoy :: Board -> Board -> Int -> Int -> Int -> Int -> StdGen -> String -> (Board, String)
stepBoy board returnBoard n m i j rand string =
    if i < n then
        if j < m then 
            if (getItem item "B")then
                stepBoy board newBoard n m i (j+1) (newStd $ newStd rand) string2
            else if isEmpty $ returnBoard ! (i,j) then
                stepBoy board (returnBoard // [((i,j), board ! (i,j))]) n m i (j+1) (newStd $ newStd rand) string
            else 
                stepBoy board returnBoard n m i (j+1) (newStd $ newStd rand) string
        else stepBoy board returnBoard n m (i+1) 0 (newStd rand) string
    else (returnBoard, string)
    where
        item = board ! (i,j)
        (newBoard, string1) = moveBoy board returnBoard n m i j (newStd $ newStd rand) ""
        string2 = string ++ string1


stepRobot :: Board -> Board -> Int -> Int -> Int -> Int -> StdGen -> String -> (Board, String)
stepRobot board returnBoard n m i j rand string =
    if i < n then
        if j < m then 
            if (getItem item "R") then
                stepRobot board newBoard n m i (j+1) (newStd $ newStd rand) string2
            else if isEmpty $ returnBoard ! (i,j) then
                stepRobot board (returnBoard // [((i,j), board ! (i,j))]) n m i (j+1) (newStd $ newStd rand) string
            else 
                stepRobot board returnBoard n m i (j+1) (newStd $ newStd rand) string
        else stepRobot board returnBoard n m (i+1) 0 (newStd rand) string
    else (returnBoard, string)
    where
        item = board ! (i,j)
        (newBoard,  string1) = moveRobot board returnBoard n m i j (newStd $ newStd rand) ""
        string2 = string ++ string1

moveBoy :: Board -> Board -> Int -> Int -> Int -> Int -> StdGen -> String -> (Board, String)
moveBoy board returnBoard n m i j rand string =
    if getItem item "B" then
        if not (getItem item "Y") && not (getItem item "L") then
            if wnt_move > 0 then
                if (length moveList) /= 0 then
                    if dir < 2 then
                        ((oldBoard // [((i,j), Item x0 x1 False x3 x4 x5),((x,y), Item False False x2 False False False)]), string3)
                    else 
                        ((newBoard // [((i,j), Item x0 x1 False x3 x4 x5),((x,y), Item False False x2 False False False)]), string4)
                else (someReturn, "[-] Boy " ++ (show (j,i)) ++ " can't move" ++ "\n")
            else (someReturn, "[-] Boy " ++ (show (j,i)) ++ " don't want move" ++ "\n")
        else (someReturn, "[-] Boy " ++ (show (j,i)) ++ " is occupied" ++ "\n")          
    else (someReturn, string)
    where
        item = board ! (i,j)
        wnt_move = randomInt 10 (newStd $ newStd rand)
        moveList = validMoveBoy board returnBoard n m i j [] 0
        temp = randomInt ((length moveList)) (newStd $ newStd rand)
        dir = moveList !! temp
        (x,y) = getDir (i,j) dir 
        (Item x0 x1 x2 x3 x4 x5) = board ! (i,j)
        (oldBoard, string1) = moveObject returnBoard returnBoard n m x y dir string
        string3 = string1 ++ "[-] Boy " ++ (show (j,i)) ++ " move to " ++ (show (y,x)) ++ "\n"
        (newBoard, string2) = moveObject board returnBoard n m x y dir string
        string4 = string2 ++ "[-] Boy " ++ (show (j,i)) ++ " move to " ++ (show (y,x)) ++ "\n"
        someReturn = (returnBoard // [((i,j), board ! (i,j))])

validMoveBoy :: Board -> Board -> Int -> Int -> Int -> Int -> [Int] -> Int -> [Int]
validMoveBoy board returnBoard n m i j move dir =
    if dir < 4 then 
        if validPosition n m x y then
            if dir < 2 then
                if (isEmpty item2) || ((getItem item2 "O") && (canMoveObject board returnBoard n m x y dir)) then 
                    validMoveBoy board returnBoard n m i j (dir:move) (dir+1)
                else validMoveBoy board returnBoard n m i j move (dir+1)
            else
                if (isEmptyAnd item1 item2) || ((getItemOr item1 item2 "O") && (canMoveObject board returnBoard n m x y dir)) then 
                    validMoveBoy board returnBoard n m i j (dir:move) (dir+1)
                else validMoveBoy board returnBoard n m i j move (dir+1)
        else validMoveBoy board returnBoard n m i j move (dir+1)
    else move
    where
        (x,y) = getDir (i,j) dir
        item1 = board ! (x,y)
        item2 = returnBoard ! (x,y)

canMoveObject :: Board -> Board -> Int -> Int -> Int -> Int -> Int -> Bool
canMoveObject board newBoard n m i j dir =
    if validPosition n m x y then
        if dir < 2 then
            if isEmpty item2 then True else
            if getItem item2 "O" then canMoveObject board newBoard n m x y dir
            else False
        else
            if isEmptyAnd item1 item2 then True else
            if getItemOr item1 item2 "O" then canMoveObject board newBoard n m x y dir
            else False
    else False
    where
        (x,y) = getDir (i,j) dir
        item1 = board ! (x,y)
        item2 = newBoard ! (x,y)

moveObject :: Board -> Board -> Int -> Int -> Int -> Int -> Int -> String -> (Board, String)
moveObject board returnBoard n m i j dir string =
    if isEmpty $ board ! (i,j) then 
        (returnBoard, string)
    else
        moveObject board (returnBoard // [((x,y), board ! (i,j))]) n m x y dir ("[-] Object " ++ (show (j,i) ++ " is moved to " ++ (show (y,x)) ++ "\n"))
    where
        (x,y) = getDir (i,j) dir

generateDirty :: Board -> Board -> Int -> Int -> Int -> Int -> StdGen -> String -> (Board, String)
generateDirty board returnBoard n m i j rand string =
    if i < (div n 3) then
        if j < (div m 3) then
            if count > 0 then
                generateDirty board (createDirty returnBoard i j countDirty (newStd $ newStd rand)) n m i (j+1) (newStd $ newStd newrand) tempString
            else generateDirty board returnBoard n m i (j+1) (newStd $ newStd newrand) tempString
        else generateDirty board returnBoard n m (i+1) 0 (newStd $ newStd newrand) string
    else (returnBoard, string)

    where
        newrand = (newStd $ newStd rand)
        count = countBoy board returnBoard (i*3+3) (j*3+3) (i*3) (j*3) 0
        empty = countEmpty returnBoard (i*3+3) (j*3+3) (i*3) (j*3) 0
        numGen = if (numberDirty count) < empty then (numberDirty count)+1 else empty
        countDirty = if numGen > 1 then randomLimInt 1 numGen (newStd newrand) else numGen

        tempString = string ++ (show (j,i)) ++ " " ++ (show count) ++ " " ++ (show empty) ++ " "  ++ (show countDirty) ++ "\n"

countBoy :: Board -> Board -> Int -> Int -> Int -> Int -> Int -> Int
countBoy board newBoard n m i j count =
    if i < n then
        if j < m then
            if (getItem (board ! (i,j)) "B") && not(getItem (newBoard ! (i,j)) "B") 
                then countBoy board newBoard n m i (j+1) (count+1)
            else countBoy board newBoard n m i (j+1) count
        else countBoy board newBoard n m (i+1) (m-3) count
    else count

countEmpty :: Board -> Int -> Int -> Int -> Int -> Int -> Int
countEmpty board n m i j count =
    if i < n then
        if j < m then
            if isEmpty $ board ! (i,j)
                then countEmpty board n m i (j+1) (count+1)
            else countEmpty board n m i (j+1) count
        else countEmpty board n m (i+1) (m-3) count
    else count

numberDirty :: Int -> Int
numberDirty x 
    | x == 1 = 1
    | x == 2 = 3
    | otherwise = 6

createDirty :: Board -> Int -> Int -> Int -> StdGen -> Board
createDirty board i j count rand  =
    if count > 0 then    
        if isEmpty $ board ! (x,y) then
            createDirty (board // [((x,y), Item True x1 x2 x3 x4 x5)]) i j (count-1) (newStd $ newStd newrand)
        else createDirty board i j count (newStd $ newStd newrand)
    else board
    where
        newrand = newStd $ newStd rand
        x = (i*3) + randomInt 3 rand
        y = (j*3) + randomInt 3 (newStd newrand)
        (Item _ x1 x2 x3 x4 x5) = board ! (x,y)

moveRobot :: Board -> Board -> Int -> Int -> Int -> Int -> StdGen -> String -> (Board, String)
moveRobot board returnBoard n m i j rand string =
    if getItem item "R" then
        if l then  -- Si esta cargando a un ninho
            if a then -- Si es un corrar
                ((returnBoard // [((i,j), Item d a b r o False)]), string0) -- Suelta a ninho
            else if (length moveYard2) > 0 then -- Si algun adyacente es corral
                ((returnBoard // [((i,j), Item d a False False o False), ((x1,y1), Item d1 a1 b r o1 l)]), string1)
            else if d then -- Si hay basura limpia
                ((returnBoard // [((i,j), Item False a b r o l)]), string00) -- Suelta a ninho
            else if (length moveRobot2) > 0 then -- Muevete1
                ((returnBoard // [((i,j), Item d a False False o False), ((x2,y2), Item d2 a2 b r o2 l)]), string2)
            else (someReturn, "[-] Robot " ++ (show (j,i)) ++ " can't move" ++ "\n")
        else if d then -- Si hay basura limpia
            ((returnBoard // [((i,j), Item False a b r o l)]), string00) -- Suelta a ninho
        else if (length moveBoy1) > 0 then -- Busca ninho
            ((returnBoard // [((i,j), Item d a b False o l), ((x3,y3), Item d3 a3 b3 r o3 True)]), string3)
        else if (length moveDirty1) > 0 then -- Busca basura
            ((returnBoard // [((i,j), Item d a b False o l), ((x4,y4), Item d4 a4 b4 r o4 False)]), string4)
        else if (length moveRobot1) > 0 then -- Muevete2
                ((returnBoard // [((i,j), Item d a b False o l), ((x5,y5), Item d5 a5 b5 r o5 b5)]), string5)
        else (someReturn, "[-] Robot " ++ (show (j,i)) ++ " can't move" ++ "\n")
    else (someReturn, string) 
    where
        item = board ! (i,j)
        (Item d a b r o l) = board ! (i,j)

        string0 = "[-] Robot " ++ (show (j,i)) ++ " leave to boy \n"
        string00 = "[-] Robot " ++ (show (j,i)) ++ " clear to derty \n"
        
        rand1 = newStd rand
        moveYard2 = validMoveYard2 board returnBoard n m i j [] 0
        (x1, y1) = moveYard2 !! (randomInt (length moveYard2) rand1)
        (Item d1 a1 b1 r1 o1 l1) = board ! (x1,y1)
        string1 = "[-] Robot " ++ (show (j,i)) ++ " move to yard " ++ (show (y1,x1)) ++ "\n"

        rand2 = newStd rand1
        moveRobot2 = validMoveRobot2 board returnBoard n m i j [] 0
        (x2, y2) = moveRobot2 !! (randomInt (length moveRobot2) rand2)
        (Item d2 a2 b2 r2 o2 l2) = board ! (x2,y2)
        string2 = "[-] Robot " ++ (show (j,i)) ++ " move1 to " ++ (show (y2,x2)) ++ "\n"

        rand3 = newStd rand2
        moveBoy1 = validMoveBoy1 board returnBoard n m i j [] 0
        (x3, y3) = moveBoy1 !! (randomInt (length moveBoy1) rand3)
        (Item d3 a3 b3 r3 o3 l3) = board ! (x3,y3)
        string3 = "[-] Robot " ++ (show (j,i)) ++ " move to Boy " ++ (show (y3,x3)) ++ "\n"

        rand4 = newStd rand3
        moveDirty1 = validMoveDirty1 board returnBoard n m i j [] 0
        (x4, y4) = moveDirty1 !! (randomInt (length moveDirty1) rand4)
        (Item d4 a4 b4 r4 o4 l4) = board ! (x4,y4)
        string4 = "[-] Robot " ++ (show (j,i)) ++ " move to dirty Item " ++ (show (y4,x4)) ++ "\n"

        rand5 = newStd rand4
        moveRobot1 = validMoveRobot1 board returnBoard n m i j [] 0
        (x5, y5, dir) = moveRobot1 !! (randomInt (length moveRobot1) rand5)
        (Item d5 a5 b5 r5 o5 l5) = if dir < 2 then returnBoard ! (x5,y5) else  board ! (x5,y5) 
        string5 = "[-] Robot " ++ (show (j,i)) ++ " move2 to " ++ (show (y5,x5)) ++ "\n"
        
        someReturn = (returnBoard // [((i,j), board ! (i,j))])

validMoveRobot :: Board -> Board -> Int -> Int -> Int -> Int -> [(Int, Int)] -> Int -> [(Int, Int)]
validMoveRobot board newBoard n m i j move dir =
    if validPosition n m x y then
        if dir < 2 then
            if not (getItem item2 "R") && not (getItem item2 "O") then
                ((x,y) : move)
            else move
        else
            if not (getItemOr item1 item2 "R") && not (getItemOr item1 item2 "O") then
                ((x,y) : move)
            else move
    else move
    where
        (x,y) = getDir (i,j) dir     
        item1 = board ! (x,y)
        item2 = newBoard ! (x,y)

validMoveRobot1 :: Board -> Board -> Int -> Int -> Int -> Int -> [(Int, Int, Int)] -> Int -> [(Int, Int, Int)]
validMoveRobot1 board newBoard n m i j move dir =
    if dir < 4 then
        if validPosition n m x y then
            if dir < 2 then
                if not (getItem item2 "R") && not (getItem item2 "O") then
                    validMoveRobot1 board newBoard n m i j ((x,y, dir):move) (dir+1)
                else validMoveRobot1 board newBoard n m i j move (dir+1)
            else
                if not (getItemOr item1 item2 "R") && not (getItemOr item1 item2 "O") then
                    validMoveRobot1 board newBoard n m i j ((x,y, dir) : move) (dir+1)
                else validMoveRobot1 board newBoard n m i j move (dir+1)
        else validMoveRobot1 board newBoard n m i j move (dir+1)
    else move
    where
        (x,y) = getDir (i,j) dir     
        item1 = board ! (x,y)
        item2 = newBoard ! (x,y)

validMoveRobot2 :: Board -> Board -> Int -> Int -> Int -> Int -> [(Int, Int)] -> Int -> [(Int, Int)]
validMoveRobot2 board newBoard n m i j move dir =
    if dir < 4 then
        if validPosition n m x y then    
            if dir < 2 then
                if not (getItem item2 "R") && not (getItem item2 "O") && not (getItem item2 "B") then
                    validMoveRobot2 board newBoard n m i j ((x,y) : newMove1) (dir+1)
                else validMoveRobot2 board newBoard n m i j move (dir+1)
            else
                if not (getItemOr item1 item2 "R") && not (getItemOr item1 item2 "O") && not (getItemOr item1 item2 "B") then
                    validMoveRobot2 board newBoard n m i j ((x,y) : newMove2) (dir+1)
                else validMoveRobot2 board newBoard n m i j move (dir+1)
        else validMoveRobot2 board newBoard n m i j move (dir+1)
    else move
    where
        (x,y) = getDir (i,j) dir     
        item1 = board ! (x,y)
        item2 = newBoard ! (x,y)

        newMove1 = if not (getItem item2 "B") then  move ++ (validMoveRobot board newBoard n m i j [] dir) else move
        newMove2 = if not (getItemOr item1 item2 "B") then  move ++ (validMoveRobot board newBoard n m i j [] dir) else move

validMoveYard :: Board -> Board -> Int -> Int -> Int -> Int -> [(Int, Int)] -> Int -> [(Int, Int)]
validMoveYard board newBoard n m i j move dir =
    if validPosition n m x y then
        if dir < 2 then
            if (getItem item2 "Y") && not (getItem item2 "B") && not (getItem item2 "R") then
                ((x,y) : move)
            else move
        else
            if (getItemOr item1 item2 "Y") && not (getItemOr item1 item2 "B") && not (getItemOr item1 item2 "R") then
                ((x,y) : move)
            else move
    else move
    where
        (x,y) = getDir (i,j) dir     
        item1 = board ! (x,y)
        item2 = newBoard ! (x,y)

validMoveYard2 :: Board -> Board -> Int -> Int -> Int -> Int -> [(Int, Int)] -> Int -> [(Int, Int)]
validMoveYard2 board newBoard n m i j move dir =
    if dir < 4 then
        if validPosition n m x y then    
            if dir < 2 then
                if (getItem item2 "Y") && (not (getItem item2 "B")) && (not (getItem item2 "R")) then
                    validMoveYard2 board newBoard n m i j ((x,y) : move) (dir+1)
                else if isEmpty item2 then
                    validMoveYard2 board newBoard n m i j newMove (dir+1)
                else
                    validMoveYard2 board newBoard n m i j move (dir+1)
            else
                if (getItemOr item1 item2 "Y") && (not (getItemOr item1 item2 "B")) && (not (getItemOr item1 item2 "R")) then
                    validMoveYard2 board newBoard n m i j ((x,y) : move) (dir+1)
                else if isEmptyAnd item1 item2 then
                    validMoveYard2 board newBoard n m i j newMove (dir+1)
                else
                    validMoveYard2 board newBoard n m i j move (dir+1)
        else validMoveYard2 board newBoard n m i j move (dir+1)
    else move
    where
        (x,y) = getDir (i,j) dir     
        item1 = board ! (x,y)
        item2 = newBoard ! (x,y)

        newMove = move ++ (validMoveYard board newBoard n m x y [] dir)

validMoveBoy1 :: Board -> Board -> Int -> Int -> Int -> Int -> [(Int, Int)] -> Int -> [(Int, Int)]
validMoveBoy1 board newBoard n m i j move dir =
    if dir < 4 then
        if validPosition n m x y then
            if dir < 2 then
                if (getItem item2 "B") && not (getItem item2 "R") && not (getItem item2 "Y") then
                    validMoveBoy1 board newBoard n m i j ((x,y) : move) (dir+1)
                else validMoveBoy1 board newBoard n m i j move (dir+1)
            else
                if (getItemOr item1 item2 "B") && not (getItemOr item1 item2 "R") && not (getItemOr item1 item2 "Y") then
                    validMoveBoy1 board newBoard n m i j ((x,y) : move) (dir+1)
                else validMoveBoy1 board newBoard n m i j move (dir+1)
        else validMoveBoy1 board newBoard n m i j move (dir+1)
    else move
    where
        (x,y) = getDir (i,j) dir     
        item1 = board ! (x,y)
        item2 = newBoard ! (x,y)

validMoveDirty1 :: Board -> Board -> Int -> Int -> Int -> Int -> [(Int, Int)] -> Int -> [(Int, Int)]
validMoveDirty1 board newBoard n m i j move dir =
    if dir < 4 then
        if validPosition n m x y then
            if dir < 2 then
                if (getItem item2 "D") && not (getItem item2 "R") && not (getItem item2 "O") then
                    validMoveDirty1 board newBoard n m i j ((x,y) : move) (dir+1)
                else validMoveDirty1 board newBoard n m i j move (dir+1)
            else
                if (getItemOr item1 item2 "D") && not (getItemOr item1 item2 "R") && not (getItemOr item1 item2 "O") then
                    validMoveDirty1 board newBoard n m i j ((x,y) : move) (dir+1)
                else validMoveDirty1 board newBoard n m i j move (dir+1)
        else validMoveDirty1 board newBoard n m i j move (dir+1)
    else move
    where
        (x,y) = getDir (i,j) dir     
        item1 = board ! (x,y)
        item2 = newBoard ! (x,y)