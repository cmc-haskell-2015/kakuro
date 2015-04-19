﻿module Main where

import Graphics.Gloss.Interface.Pure.Game
import Control.Applicative
import Data.Char
import Data.String

data World = World [[Cell]] String Float Float Int
data Cell = White Int | Black Int Int 

-------------the main function, loading from file and creatint field--------------------------

main :: IO ()
main = do
  src <- readFile "input.txt"
  createWorld (World (handleInput src) "You can enter the numbers from 1 to 9" 500 500 0)  
  
  
createWorld :: World -> IO()
createWorld world = play (InWindow "Kakuro" (500, 500) (20, 20))
                (makeColor 1 1 1 1)
                50
                world
                converter
                handler
                updater
                                
converter (World s z x y n) = Pictures[createTopText z, createAllRows s 0]

------------------------------------all field-------------------------------------------------------------------

createTopText :: String -> Picture
createTopText s = translate (-200) (220) (scale 0.12 0.12 (text s))

createAllRows :: [[Cell]] -> Float -> Picture
createAllRows [] _ = Blank
createAllRows (x : xs) n = Pictures[(createOneRow x n 0), (createAllRows xs (n + 1))]

-----------------------------------one row of field------------------------------------

createOneRow :: [Cell] -> Float -> Float -> Picture
createOneRow [] _ _ = Blank
createOneRow (x : xs) i j = Pictures[(createOneCell x i j), (createOneRow xs i (j + 1))]

----------------------------------create one cell - white or black: create rectangle and text---------------------

createOneCell :: Cell -> Float -> Float -> Picture
createOneCell (White x) i j = Pictures[(cellTextWhite x i j), (whiteRectangle i j)]
createOneCell (Black x y) i j =  Pictures[(cellTextBlack x y i j), (blackRectangle i j)]

whiteRectangle :: Float -> Float ->Picture
whiteRectangle i j = translate  (-175 + j * 50) (175 - i * 50) (rectangleWire 50 50)

blackRectangle :: Float -> Float -> Picture
blackRectangle i j  =  Pictures[color (makeColor 0.7 0.7 0.7 0.7) (translate  (-175 + j * 50) (175 - i * 50) (rectangleSolid 50 50)),
                    translate  (-175 + j * 50) (175 - i * 50) (rectangleWire 50 50),
                    line[ (-200 + 50 * j, 200 - 50 * i), (-150 + j * 50, 150 - i * 50)]]

cellTextWhite :: Int -> Float -> Float -> Picture
cellTextWhite 0 _ _ = Blank
cellTextWhite n i j = Pictures[translate (-175 + j * 50 - 5) (175 - i * 50 - 5) (scale 0.2 0.2 (text (intToString n))),
                      color (makeColor 0.94 0.94 0.58 0.7) (translate  (-175 + j * 50) (175 - i * 50) (rectangleSolid 50 50))]
                      
--------------------------------convert int to string--------------------------------------------------------------------------------------

intToString :: Int -> String
intToString n | n < 10 = [intToDigit n]
              | otherwise = [intToDigit (div n 10), intToDigit(mod n 10)]

cellTextBlack :: Int -> Int -> Float -> Float -> Picture
cellTextBlack 99 99 _ _ = Blank
cellTextBlack 0 y i j = translate  (-175 + j * 50 + 5) (175 - i * 50 + 5) (scale 0.1 0.1 (text (intToString y)))
cellTextBlack x 0 i j = translate  (-175 + j * 50 - 15) (175 - i * 50 - 15) (scale 0.1 0.1 (text (intToString x)))
cellTextBlack x y i j = Pictures[translate  (-175 + j * 50 + 5) (175 - i * 50 + 5) (scale 0.1 0.1 (text (intToString y))),
                        translate  (-175 + j * 50 - 15) (175 - i * 50 - 15) (scale 0.1 0.1 (text (intToString x)))]
                        
--------------------------------------the event handler: mouse left button and 1-9 numbers buttons-----------------------------------------------
                
handler :: Event -> World -> World
handler (EventKey (MouseButton LeftButton) Down _ (i, j)) (World s z x y n) = World s z i j 0
handler (EventKey (Char c) Down _ _) (World s z x y n) | c >= '1' && c <= '9' = putCell (World s z x y (digitToInt c))
                                                       | otherwise = (World s z x y 0)
handler (EventKey (SpecialKey KeyEnter) Down _ _) (World s z x y n) = World s (answerForUser (World s z x y n)) 500 500 0
handler _ world = world                

------------------------------------------add permission number to white cell----------------------------------------------

putCell :: World -> World
putCell (World s z x y n) | x > -200 && x < 200 && y > -200 && y < 200 = 
                                (World (new_cells  s (7 - (div ((round y) + 200) 50)) (div  ((round x) + 200) 50) n) z 500 500 0)
                          | otherwise = World s z 500 500 0

new_cells :: [[Cell]] -> Int -> Int -> Int -> [[Cell]]
new_cells (s : sx) 0 y n = (new_cell s y n) : sx 
new_cells (s : sx) x y n = s : (new_cells sx (x - 1) y n)  

new_cell :: [Cell] -> Int -> Int -> [Cell]
new_cell ((White x) : xs) 0  n = (White n) : xs
new_cell (x : xs) y n = x : (new_cell xs (y - 1) n)
new_cell s x n = s

------------------------------------------end of the game: create answer to user about win----------------------------------------------

answerForUser :: World -> String
answerForUser (World s z x y n) = "You won! Or not won.. I don't know"

----------------------------------------empty updater---------------------------------------------------------------------------
                
updater _ world = world

-------------------------------------------------------creating array of cells into text file---------------------------------

handleInput :: String -> [[Cell]]
handleInput src = handleLines (lines src)

handleLines :: [String] -> [[Cell]]
handleLines [] = []
handleLines (x:xs) = (handleWords (words x)) : (handleLines xs)

handleWords :: [String] -> [Cell]
handleWords [] = []
handleWords (x : y : xs) = (makeCell (toInt x 0) (toInt y 0)) : (handleWords xs)

makeCell :: Int -> Int -> Cell
makeCell 0 0 = White 0
makeCell t1 t2 = Black t1 t2

toInt :: String -> Int -> Int
toInt [] s = s
toInt (x:xs) s = (toInt xs (s * 10 + (digitToInt x)))

