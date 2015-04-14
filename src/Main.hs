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
  print (playing (handleInput src))
  
getFirstInt :: String -> Int
getFirstInt [] = 0
getFirstInt (x: xs) = digitToInt x

playing :: [[Cell]] -> Int
playing (x : xs) = ololo (head x)

ololo :: Cell -> Int
ololo (White x) = x
ololo (Black x y) = y

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

