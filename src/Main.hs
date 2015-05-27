module Main where

import Graphics.Gloss.Interface.IO.Game
import Control.Applicative()
import Data.Char
import Data.String()
import Data.List

data World = World [[Cell]] String Float Float Int String Int
data Cell = White Int | Black Int Int 

-------------the main function, loading from file and creatint field-----------

main :: IO ()
main = do
    src <- readFile "../examples/input.txt"
    createWorld (World (handleInput src) "You can enter the numbers from 1 to 9" (-1) (-1) 0 "Select cell and press F1 to show the hint" 0)

  
createWorld :: World -> IO()
createWorld world = do
                playIO (InWindow "Kakuro" (500, 600) (20, 20)) (makeColor 1 1 1 1) 50 world converter handler updater
                
                
converter :: World -> IO Picture
converter (World s z a b _ prompt flag) = return (Pictures[createTopText z, createAllRows s 0 a b, createPrompt prompt, getButtons])

-----------------------------------buttons-----------------------------------------------

getButtons :: Picture
getButtons = Pictures[saveButton, loadButton, newGameButton]

saveButton :: Picture
saveButton = Pictures[translate (-175) (-260) (scale 0.17 0.17 (text "SAVE")),
                color (makeColor 0.6 0.6 0.94 0.7) (translate  (-150) (-250) (rectangleSolid 100 50)),
                color (makeColor 0.7 0.7 1 0.7) (translate  (-150) (-250) (rectangleWire 100 50))]
                
loadButton :: Picture
loadButton = Pictures[translate (-25) (-260) (scale 0.17 0.17 (text "LOAD")),
                color (makeColor 0.6 0.6 0.94 0.7) (translate  0 (-250) (rectangleSolid 100 50)),
                color (makeColor 0.7 0.7 1 0.7) (translate  0 (-250) (rectangleWire 100 50))]
                
newGameButton :: Picture
newGameButton = Pictures[translate 110 (-257) (scale 0.12 0.12 (text "NEW GAME")),
                color (makeColor 0.6 0.6 0.94 0.7) (translate  150 (-250) (rectangleSolid 100 50)),
                color (makeColor 0.7 0.7 1 0.7) (translate  150 (-250) (rectangleWire 100 50))]


------------------------------------all field---------------------------------------------

createTopText :: String -> Picture
createTopText s = translate (-200) (220) (scale 0.12 0.12 (text s))

createAllRows :: [[Cell]] -> Float -> Float -> Float -> Picture
createAllRows [] _ _ _= Blank
createAllRows (x : xs) n a b = Pictures[(createOneRow x n 0 a b), (createAllRows xs (n + 1) a b)]

-----------------------------------one row of field------------------------------------

createOneRow :: [Cell] -> Float -> Float -> Float -> Float -> Picture
createOneRow [] _ _ _ _ = Blank
createOneRow (x : xs) i j a b = Pictures[(createOneCell x i j a b), (createOneRow xs i (j + 1) a b)]

--------------------create one cell - white or black: create rectangle and text---------

createOneCell :: Cell -> Float -> Float -> Float -> Float -> Picture
createOneCell (White x) i j a b | (i == a) && (j == b) && x == 0 = Pictures[(whiteRectangle i j),
                                                                            (greenRectangle i j)]
                                | (i == a) && (j == b) = Pictures[(cellTextWhite x i j),
                                                                    (whiteRectangle i j),
                                                                    (greenRectangle i j)]
                                | x == 0 = whiteRectangle i j
                                | otherwise = Pictures[(cellTextWhite x i j),
                                                        (whiteRectangle i j),
                                                        (yellowRectangle i j)]
createOneCell (Black x y) i j _ _ =  Pictures[(cellTextBlack x y i j),(blackRectangle i j)]

whiteRectangle :: Float -> Float -> Picture
whiteRectangle i j = translate  (-175 + j * 50) (175 - i * 50) (rectangleWire 50 50)

greenRectangle :: Float -> Float -> Picture
greenRectangle i j = color (makeColor 0.7 0.9 0.7 0.7) (translate  (-175 + j * 50) (175 - i * 50) 
                                (rectangleSolid 50 50))

blackRectangle :: Float -> Float -> Picture
blackRectangle i j  =  Pictures[color (makeColor 0.7 0.7 0.7 0.7) 
                    (translate  (-175 + j * 50)(175 - i * 50) (rectangleSolid 50 50)),
                    translate  (-175 + j * 50) (175 - i * 50) (rectangleWire 50 50),
                    line[ (-200 + 50 * j, 200 - 50 * i), (-150 + j * 50, 150 - i * 50)]]
                    
yellowRectangle :: Float -> Float -> Picture
yellowRectangle i j = color (makeColor 0.94 0.94 0.58 0.7) (translate  (-175 + j * 50) (175 - i * 50)
                                (rectangleSolid 50 50))               

cellTextWhite :: Int -> Float -> Float -> Picture
cellTextWhite 0 _ _ = Blank
cellTextWhite n i j = translate (-175 + j * 50 - 5) (175 - i * 50 - 5) (scale 0.2 0.2 (text (intToString n)))

---------------------------------image of prompt-----------------------------------

createPrompt :: String -> Picture
createPrompt s = translate (-200) (260) (scale 0.12 0.12 (text s))
                      
--------------------------------convert int to string--------------------------------

intToString :: Int -> String
intToString n | n < 10 = [intToDigit n]
              | otherwise = [intToDigit (div n 10), intToDigit(mod n 10)]

cellTextBlack :: Int -> Int -> Float -> Float -> Picture
cellTextBlack 0 0 _ _ = Blank
cellTextBlack 0 y i j = translate  (-175 + j * 50 + 5) (175 - i * 50 + 5) 
            (scale 0.1 0.1 (text (intToString y)))
cellTextBlack x 0 i j = translate  (-175 + j * 50 - 15) (175 - i * 50 - 15)
            (scale 0.1 0.1 (text (intToString x)))
cellTextBlack x y i j = Pictures[translate  (-175 + j * 50 + 5) (175 - i * 50 + 5)
            (scale 0.1 0.1 (text (intToString y))),
             translate  (-175 + j * 50 - 15) (175 - i * 50 - 15)
            (scale 0.1 0.1 (text (intToString x)))]
                        
---------------the event handler: mouse left button and 1-9 numbers buttons---------------

handler :: Event -> World -> IO World
handler ev w = return (handlerDecorator ev w)

handlerDecorator :: Event -> World -> World
handlerDecorator (EventKey (MouseButton LeftButton) Down _ (i, j)) (World s z x y n prompt flag) 
                    | (i > -200) && (i < -100) && (j > -275) && (j < -225) = World s z x y n prompt 1
                    | (i > -50) && (i < 50) && (j > -275) && (j < -225) = World s z x y n prompt 2
                    | (i > 100) && (i < 200) && (j > -275) && (j < -225) = World s z x y n prompt 3
                    | otherwise = World s z (rowsNumber j) (colsNumber i) 0 prompt flag
handlerDecorator (EventKey (Char c) Down _ _) (World s z x y _ prompt flag) 
                | c >= '1' && c <= '9' = putCell (World s z x y (digitToInt c) prompt flag)
                | otherwise = (World s z x y 0 prompt flag)
handlerDecorator (EventKey (SpecialKey KeyEnter) Down _ _) (World s z x y n prompt flag) =
                World s (answerForUser s) (-1) (-1) 0 prompt flag
handlerDecorator (EventKey (SpecialKey KeyF1) Down _ _) (World s z x y n _ flag) =
                World s z x y n (generatePrompt s x y) flag
handlerDecorator _ world = world

------------------row's and column's number--------------------------------------

rowsNumber :: Float -> Float
rowsNumber x | x < -200 || x > 200 = (-1)
             | otherwise = fromInteger (7 - (div ((round x) + 200) 50))
             
colsNumber :: Float -> Float
colsNumber x | x < -200 || x > 200 = (-1)
             | otherwise = fromInteger (div ((round x) + 200) 50)
             

------------------add permission number to white cell----------------

putCell :: World -> World
putCell (World s z x y n prompt flag) | x == (-1) || y == (-1) = World s z (-1) (-1) 0 prompt flag
                          | otherwise = (World (newCells s x y n) z (-1) (-1) 0 prompt flag)

newCells :: [[Cell]] -> Float -> Float -> Int -> [[Cell]]
newCells [] _ _ _ = []
newCells (s : sx) 0 y n = (newCell s y n) : sx 
newCells (s : sx) x y n = s : (newCells sx (x - 1) y n)  

newCell :: [Cell] -> Float -> Int -> [Cell]
newCell ((White _) : xs) 0  n = (White n) : xs
newCell (x : xs) y n = x : (newCell xs (y - 1) n)
newCell s _ _ = s

------------------------end of the game: create answer to user about win--------------

answerForUser :: [[Cell]] -> String
answerForUser s = createAnswer (checkCells s s 0)

-----------------------create error message or phrase "You won"-------------------

createAnswer :: String -> String
createAnswer "" = "You won!"
createAnswer s = s

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
checkCell _ (Black 0 0) _ _ = ""
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

-------------------------empty updater----------------------------
                
updater :: Float -> World -> IO World
updater _ (World s z x y n prompt 1) = do
            saveGame s
            return (World s z x y n prompt 0)
updater _ (World s z x y n prompt 2) = do
            src <- readFile "../examples/lastGame.txt"
            return (World (handleInput src) "You can enter the numbers from 1 to 9" (-1) (-1) 0 "Select cell and press F1 to show the hint" 0)
updater _ (World s z x y n prompt 3) = do
            src <- readFile "../examples/input.txt"
            return (World (handleInput src) "You can enter the numbers from 1 to 9" (-1) (-1) 0 "Select cell and press F1 to show the hint" 0)
updater _ world = do
            return world


-------------creating array of cells into text file----------------

handleInput :: String -> [[Cell]]
handleInput src = handleLines (lines src)

handleLines :: [String] -> [[Cell]]
handleLines [] = []
handleLines (x:xs) = (handleWords (words x)) : (handleLines xs)

handleWords :: [String] -> [Cell]
handleWords [] = []
handleWords [_] = []
handleWords ("W" : y : xs) = (makeWhiteCell (toInt y 0)) : (handleWords xs)
handleWords ("B" : x : y : xs) = (makeBlackCell (toInt x 0) (toInt y 0)) : (handleWords xs)

makeWhiteCell :: Int -> Cell
makeWhiteCell x = White x

makeBlackCell :: Int -> Int -> Cell
makeBlackCell x y = Black x y

toInt :: String -> Int -> Int
toInt [] s = s
toInt (x:xs) s = (toInt xs (s * 10 + (digitToInt x)))

-------------------------------------------------------------------------
---------------generation of prompt--------------------------------------

elementAt :: [a] -> Float -> a
elementAt l i = last (take ((round i) + 1) l)

isBlack :: Cell -> Bool
isBlack (Black _ _) = True
isBlack _ = False

listIntToString :: [Int] -> String
listIntToString [] = "your solution is wrong yet" 
listIntToString [x] = intToString x
listIntToString (x : xs) = (intToString x) ++ " " ++ (listIntToString xs)

intersectList :: [Int] -> [Int] -> [Int]
intersectList a b = filter (\x -> (elem x a)) b

------------------------------------------------------------------------------

generatePrompt :: [[Cell]] -> Float -> Float -> String
generatePrompt s x y | (x == -1) || (y == -1) || (isBlack (elementAt (elementAt s x) y)) 
                                = "Select cell and press F1 to show the hint"

                     | otherwise = "Hint: " ++ listIntToString (intersectList (generatePromptHor s x y)
                                                                (generatePromptVer (transpose s) y x))

generatePromptHor :: [[Cell]] -> Float -> Float -> [Int]
generatePromptHor s x y = exclusionPromptHor (elementAt s x) (elementAt (elementAt s x) y) y y
                                                    
exclusionPromptHor :: [Cell] -> Cell -> Float -> Float -> [Int]
exclusionPromptHor cells (Black _ n) y posY = exclusionRightPrompt 
                             (drop ((round y) + 1) cells) [1..9] n (posY - y - 1)
exclusionPromptHor cells (White _) y posY = exclusionPromptHor
                              cells (elementAt cells (y - 1)) (y - 1) posY
                                                    
generatePromptVer :: [[Cell]] -> Float -> Float -> [Int]
generatePromptVer s x y = exclusionPromptVer (elementAt s x) (elementAt (elementAt s x) y) y y

exclusionPromptVer :: [Cell] -> Cell -> Float -> Float -> [Int]
exclusionPromptVer cells (Black n _) y posY = exclusionRightPrompt 
                             (drop ((round y) + 1) cells) [1..9] n (posY - y - 1)
exclusionPromptVer cells (White _) y posY = exclusionPromptVer
                              cells (elementAt cells (y - 1)) (y - 1) posY

exclusionRightPrompt :: [Cell] -> [Int] -> Int -> Float -> [Int]
exclusionRightPrompt [] list sum posY = filter (\x -> x <= sum) list
exclusionRightPrompt ((Black _ _) : xs) list sum posY = filter (\x -> x <= sum) list
exclusionRightPrompt ((White x) : xs) list sum 0 = exclusionRightPrompt xs list sum (-1)
exclusionRightPrompt ((White 0) : xs) list sum posY = 
                            exclusionRightPrompt xs list (sum - 1) (posY - 1)
exclusionRightPrompt ((White x) : xs) list sum posY = 
                            exclusionRightPrompt xs (delete x list) (sum - x) (posY - 1)

--------------------------------------------------------------------------

--------------------------------save games------------------------------------
------------------------------------------------------------------------------

saveGame :: [[Cell]] -> IO()
saveGame s = writeFile "../examples/lastGame.txt" (saveAllRows s)

saveAllRows :: [[Cell]] -> String
saveAllRows [] = ""
saveAllRows (x : xs) = (saveRow x) ++ "\n" ++ (saveAllRows xs)

saveRow :: [Cell] -> String
saveRow [] = ""
saveRow ((White x) : xs) = "W " ++ (intToString x) ++ " " ++ (saveRow xs)
saveRow ((Black x y) : xs) = "B " ++ (intToString x) ++ " " ++
                    (intToString y) ++ " " ++ (saveRow xs)


