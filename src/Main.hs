module Main where

import Graphics.Gloss.Interface.Pure.Game
import Control.Applicative()
import Data.Char
import Data.String()


data World = World [[Cell]] String Float Float Int Float 
data Cell = White Int | Black Int Int 

-------------the main function, loading from file and creatint field-----------

main :: IO ()
main = do
    src <- readFile "../examples/input.txt"
    createWorld (World (handleInput src) "You can enter the numbers from 1 to 9" 500 500 0 0)  
  
  
createWorld :: World -> IO()
createWorld world = play (InWindow "Kakuro" (650, 650) (20, 20))
                (makeColor 1 1 1 1)
                1
                world
                converter
                handler
                updater
                
                
converter :: World -> Picture                       
converter (World s z _ _ _ t)| z ==  "You can enter the numbers from 1 to 9"  = Pictures[createbut t, createTopText z, createAllRows s 0]
                             | otherwise = Pictures[createstop  z t]

createstop :: String -> Float -> Picture
createstop z t = Pictures[rectangleWire 300 100, translate (-75) (-15) (scale 0.3 0.3 (text (print_time t))), translate (-200) (150) (scale 0.3 0.3 (text z))]


-------------------------empty updater----------------------------
                
updater :: Float -> World -> World
updater tim (World s z x y n t)| z ==  "You can enter the numbers from 1 to 9"  = (World s z x y n  (t + tim))
                               | otherwise = (World s z x y n t)

-----------------------my----------------------------------------


createbut :: Float -> Picture
createbut s = Pictures[translate  0 275 (rectangleWire 200 50), translate (-50) 265 (scale 0.2 0.2 (text (print_time s)))]


print_time :: Float-> String 
print_time s =  hour s ++ ":"  ++ minute s ++ ":" ++ seconds s

hour :: Float -> String
hour s = check (div (truncate s) 3600)

minute :: Float -> String
minute s = check (div (mod (truncate s) 3600) 60)

seconds :: Float -> String
seconds s = check(mod (mod (truncate s) 3600) 60)

check  :: Int -> String
check n | n < 10 = "0" ++ show n
        | otherwise = show n

------------------------------------all field---------------------------------------------

createTopText :: String -> Picture
createTopText s = translate (-200) (220) (scale 0.12 0.12 (text s))

createAllRows :: [[Cell]] -> Float -> Picture
createAllRows [] _ = Blank
createAllRows (x : xs) n = Pictures[(createOneRow x n 0), (createAllRows xs (n + 1))]

-----------------------------------one row of field------------------------------------

createOneRow :: [Cell] -> Float -> Float -> Picture
createOneRow [] _ _ = Blank
createOneRow (x : xs) i j = Pictures[(createOneCell x i j), (createOneRow xs i (j + 1))]

--------------------create one cell - white or black: create rectangle and text---------

createOneCell :: Cell -> Float -> Float -> Picture
createOneCell (White x) i j = Pictures[(cellTextWhite x i j), (whiteRectangle i j)]
createOneCell (Black x y) i j =  Pictures[(cellTextBlack x y i j), (blackRectangle i j)]

whiteRectangle :: Float -> Float ->Picture
whiteRectangle i j = translate  (-175 + j * 50) (175 - i * 50) (rectangleWire 50 50)

blackRectangle :: Float -> Float -> Picture
blackRectangle i j  =  Pictures[color (makeColor 0.7 0.7 0.7 0.7) 
                    (translate  (-175 + j * 50)(175 - i * 50) (rectangleSolid 50 50)),
                    translate  (-175 + j * 50) (175 - i * 50) (rectangleWire 50 50),
                    line[ (-200 + 50 * j, 200 - 50 * i), (-150 + j * 50, 150 - i * 50)]]

cellTextWhite :: Int -> Float -> Float -> Picture
cellTextWhite 0 _ _ = Blank
cellTextWhite n i j = Pictures[translate (-175 + j * 50 - 5) (175 - i * 50 - 5)
                    (scale 0.2 0.2 (text (intToString n))),
                    color (makeColor 0.94 0.94 0.58 0.7) 
                    (translate  (-175 + j * 50) (175 - i * 50) (rectangleSolid 50 50))]
                      
--------------------------------convert int to string--------------------------------

intToString :: Int -> String
intToString n | n < 10 = [intToDigit n]
              | otherwise = [intToDigit (div n 10), intToDigit(mod n 10)]

cellTextBlack :: Int -> Int -> Float -> Float -> Picture
cellTextBlack 99 99 _ _ = Blank
cellTextBlack 0 y i j = translate  (-175 + j * 50 + 5) (175 - i * 50 + 5) 
            (scale 0.1 0.1 (text (intToString y)))
cellTextBlack x 0 i j = translate  (-175 + j * 50 - 15) (175 - i * 50 - 15)
            (scale 0.1 0.1 (text (intToString x)))
cellTextBlack x y i j = Pictures[translate  (-175 + j * 50 + 5) (175 - i * 50 + 5)
            (scale 0.1 0.1 (text (intToString y))),
             translate  (-175 + j * 50 - 15) (175 - i * 50 - 15)
            (scale 0.1 0.1 (text (intToString x)))]
                        
---------------the event handler: mouse left button and 1-9 numbers buttons---------------
                
handler :: Event -> World -> World
handler (EventKey (MouseButton LeftButton) Down _ (i, j)) (World s z _ _ _ t) = 
                World s z i j 0 t
handler (EventKey (Char c) Down _ _) (World s z x y _ t)  
                | c >= '1' && c <= '9' = putCell (World s z x y (digitToInt c) t)
                | otherwise = (World s z x y 0 t)
handler (EventKey (SpecialKey KeyEnter) Down _ _) (World s z x y n t) = 
                World s (answerForUser (World s z x y n t)) 500 500 0 t
handler _ world = world                

------------------add permission number to white cell----------------

putCell :: World -> World
putCell (World s z x y n t) | x > -200 && x < 200 && y > -200 && y < 200 = 
            (World (newCells  s (7 - (div ((round y) + 200) 50))
            (div  ((round x) + 200) 50) n) z 500 500 0 t)
                          | otherwise = World s z 500 500 0  t

newCells :: [[Cell]] -> Int -> Int -> Int -> [[Cell]]
newCells [] _ _ _ = []
newCells (s : sx) 0 y n = (newCell s y n) : sx 
newCells (s : sx) x y n = s : (newCells sx (x - 1) y n)  

newCell :: [Cell] -> Int -> Int -> [Cell]
newCell ((White _) : xs) 0  n = (White n) : xs
newCell (x : xs) y n = x : (newCell xs (y - 1) n)
newCell s _ _ = s

------------------------end of the game: create answer to user about win--------------

answerForUser :: World -> String
answerForUser (World s _ _ _ _ t) = createAnswer (checkCells s s 0) 

-----------------------create error message or phrase "You won"-------------------

createAnswer :: String -> String
createAnswer ""  = "You won!" 
createAnswer s  = s

handleString :: String -> String -> String
handleString "" "" = ""
handleString "" s = s
handleString s _ = s

------------------------recurse cells-----------------------------------------

checkCells :: [[Cell]] -> [[Cell]] -> Int -> String
checkCells _ [] _ = ""
checkCells s (x : xs) i = handleString (checkRow s x i 0) (checkCells s xs (i + 1))

checkRow :: [[Cell]] -> [Cell] -> Int -> Int -> String
checkRow _ [] _ _ = ""
checkRow s (x : xs) i j = handleString (checkCell s x i j) (checkRow s xs i (j + 1))

---------------------------check one cell----------------------------

checkCell :: [[Cell]] -> Cell -> Int -> Int -> String
checkCell _ (White _) _ _ = ""
checkCell _ (Black 99 99) _ _ = ""
------(i, j)-cell is black, next cell is (i, j + 1) for horizontal
                                --and (i + 1, j) for vertical-----
checkCell s (Black 0 y) i j = horizontalBlock s y i (j + 1)
checkCell s (Black x 0) i j = verticalBlock s x (i + 1) j
checkCell s (Black x y) i j = handleString (verticalBlock s x (i + 1) j)
                                           (horizontalBlock s y i (j + 1))

---------------------------check blocks, count summa-----------------------------

horizontalBlock :: [[Cell]] -> Int -> Int -> Int -> String
horizontalBlock [] _ _ _ = ""
horizontalBlock (x : _) summa 0 j = handleHorBlock x summa j []
horizontalBlock (_ : xs) summa i j = horizontalBlock xs summa (i - 1) j

handleHorBlock :: [Cell] -> Int -> Int -> [Int] -> String
handleHorBlock _ _ _ [0] = "Number can't be repeated"
handleHorBlock [] 0 _ _ = ""
handleHorBlock [] _ _ _ = "Wrong solution. Try again"
handleHorBlock ((Black _ _) : _) 0 0 _ = ""
handleHorBlock ((Black _ _) : _) _ 0 _ = "Wrong solution. Try again"
handleHorBlock ((White 0) : _) _ 0 _ = "Some cells are not filled"
handleHorBlock ((White x) : xs) summa 0 l = handleHorBlock xs (summa - x) 0 (addNumberToL x l)
handleHorBlock (_ : xs) summa j l = handleHorBlock xs summa (j - 1) l


verticalBlock :: [[Cell]] -> Int -> Int -> Int -> String
verticalBlock [] _ _ _ = ""
verticalBlock (x : xs) summa 0 j  = handleVerBlock xs (findVertCell x j) summa j []
verticalBlock (_ : xs) summa i j  = verticalBlock xs summa (i - 1) j

findVertCell :: [Cell] -> Int -> Cell
findVertCell [] _ = (White 0)
findVertCell (x : _) 0 = x
findVertCell (_ : xs) j = findVertCell xs (j - 1)

handleVerBlock :: [[Cell]] -> Cell -> Int -> Int -> [Int] -> String
handleVerBlock _ _ _ _ [0] = "Number can't be repeated"
handleVerBlock _ (Black _ _) 0 _ _ = ""
handleVerBlock _ (Black _ _) _ _ _ = "Wrong solution. Try again"
handleVerBlock _ (White 0) _ _ _ = "Some cells are not filled"
handleVerBlock [] (White x) summa j l = handleVerBlock [] (Black x 0) (summa - x) j 
                                            (addNumberToL x l)
handleVerBlock (m : ms) (White x) summa j l = handleVerBlock ms (findVertCell m j)
                                            (summa - x) j (addNumberToL x l)
                                            
---------------------addition to list and check for matching numbers----

addNumberToL :: Int -> [Int] -> [Int]
addNumberToL n [] = [n]
addNumberToL n (x : xs) | n == x = [0]
                        | otherwise = (x : (addNumberToL n xs))


-------------creating array of cells into text file----------------

handleInput :: String -> [[Cell]]
handleInput src = handleLines (lines src)

handleLines :: [String] -> [[Cell]]
handleLines [] = []
handleLines (x:xs) = (handleWords (words x)) : (handleLines xs)

handleWords :: [String] -> [Cell]
handleWords [] = []
handleWords [_] = []
handleWords (x : y : xs) = (makeCell (toInt x 0) (toInt y 0)) : (handleWords xs)

makeCell :: Int -> Int -> Cell
makeCell 0 0 = White 0
makeCell t1 t2 = Black t1 t2

toInt :: String -> Int -> Int
toInt [] s = s
toInt (x:xs) s = (toInt xs (s * 10 + (digitToInt x)))

