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

createAllRows :: [[Cell]] -> Float -> Picture
createAllRows [] _ = Blank
createAllRows (x : xs) n = Pictures[(createOneRow x n 0), (createAllRows xs (n + 1))]

--------------отрисовка одной строки с клетками. параметры: строка, номер строки, номер столбца-----------

createOneRow :: [Cell] -> Float -> Float -> Picture
createOneRow [] _ _ = Blank
createOneRow (x : xs) i j = Pictures[(createOneCell x i j), (createOneRow xs i (j + 1))]

--------------отрисовка одной клетки. параметры: клетка, номер строки, номер столбца-----------------------

createOneCell :: Cell -> Float -> Float -> Picture
createOneCell (White x) i j = Pictures[(whiteRectangle i j), (cellTextWhite x i j)]
createOneCell (Black x y) i j =  Pictures[(blackRectangle i j), (cellTextBlack x y i j)]

whiteRectangle :: Float -> Float ->Picture
whiteRectangle i j = translate  (-175 + j * 50) (175 - i * 50) (rectangleWire 50 50)

blackRectangle :: Float -> Float -> Picture
blackRectangle i j  =  Pictures[color (makeColor 0.7 0.7 0.7 0.7) (translate  (-175 + j * 50) (175 - i * 50) (rectangleSolid 50 50)),
                    translate  (-175 + j * 50) (175 - i * 50) (rectangleWire 50 50),
                    line[ (-200 + 50 * j, 200 - 50 * i), (-150 + j * 50, 150 - i * 50)]]

cellTextWhite :: Int -> Float -> Float -> Picture
cellTextWhite 0 _ _ = Blank
cellTextWhite n i j = translate (-175 + j * 50) (175 - i * 50) (scale 0.2 0.2 (text (intToString n)))

intToString :: Int -> String
intToString n | n < 10 = [intToDigit n]
              | n >= 10 = [intToDigit (div n 10), intToDigit(mod n 10)]

cellTextBlack :: Int -> Int -> Float -> Float -> Picture
cellTextBlack 99 99 _ _ = Blank
cellTextBlack 0 y i j = translate  (-175 + j * 50 + 5) (175 - i * 50 + 5) (scale 0.1 0.1 (text (intToString y)))
cellTextBlack x 0 i j = translate  (-175 + j * 50 - 15) (175 - i * 50 - 15) (scale 0.1 0.1 (text (intToString x)))
cellTextBlack x y i j = Pictures[translate  (-175 + j * 50 + 5) (175 - i * 50 + 5) (scale 0.1 0.1 (text (intToString y))),
                        translate  (-175 + j * 50 - 15) (175 - i * 50 - 15) (scale 0.1 0.1 (text (intToString x)))]
                
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

