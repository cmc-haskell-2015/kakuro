module Main where

import Graphics.Gloss.Interface.Pure.Game
import Control.Applicative
import Data.Char
import Data.String

data Cell = White Int | Black Int Int

main :: IO ()
main = do
  src <- readFile "input.txt"
  --writeFile "output.txt" src
  createWorld(handleInput src)
  
---------------отрисовка всего поля-------------------------------
  
createWorld :: [[Cell]] -> IO()
createWorld world = play (InWindow "Kakuro" (500, 500) (20, 20))
                (makeColor 1 1 1 1)
                50
                world
                converter
                handler
                updater
                                
converter s = createAllRows s 0

-------------отрисовка всех клеток-----------------------

createAllRows :: [[Cell]] -> Int -> Picture
createAllRows [] _ = Blank
createAllRows (x : xs) n = Pictures[(createOneRow x n 0), (createAllRows xs (n + 1))]

--------------отрисовка одной строки с клетками. параметры: строка, номер строки, номер столбца-----------

createOneRow :: [Cell] -> Int -> Int -> Picture
creaeOneRow [] _ _ = Blank
createOneRow (x : xs) i j = Pictures[(createOneCell x i j), (createOneRow xs i (j + 1))]

--------------отрисовка одной клетки. параметры: клетка, номер строки, номер столбца-----------------------

createOneCell :: Cell -> Int -> Int -> Picture
createOneCell (White x) i j = Pictures[(whiteRectangle i j), (cellText x)]
createOneCell (Black x y) i j =  Pictures[(blackRectangle i j), (cellTextBlack x y)]

whiteRectangle :: Int -> Int ->Pictures
whiteRectangle i j = translate  (-175 + j * 50) (175 - i * 50) (rectangleWire 50 50)

blackRectangle :: Int -> Int -> Picture
blackRectangle i j x y  =  pictures[translate  (-175 + j * 50) (175 - i * 50) (rectangleWire 50 50), line[ (-200 + 50 * j, 200 - 50 * i), (-150 + j * 50, 150 - i * 50)]]



--pictures[(color (makeColor 0 0 0 0) (scale 1 1 (translate 0 0 (text "Hello")))), (rectangleWire 50 70)]
-----line [(0, 0), (50, 70)]
-----translate 70 100 (rectangleWire 50 70)
                
handler _ world = world
                
updater _ world = world
   
---------------обработка входного файла: возврат матрицы со значениями в клетках поля---------------
  
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

