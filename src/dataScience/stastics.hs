{-# LANGUAGE FlexibleContexts #-}

module Stastics where

import           Data.Heap
import           Data.Maybe                  (fromJust)

import qualified Data.Vector.Unboxed         as U
import           Graphics.EasyPlot
import           Statistics.LinearRegression



-- Moving/Running Mean



-- convert raw data to Double
clean raw = map (\s -> read s :: Double) (lines raw)

-- Calculate moving average -- hardcoded smoothening factor = 0.95
avg :: [Double] -> Double
avg xs = case xs of
    (x:xs) -> a * x + (1 - a) * (avg xs)
    []     -> 0
    where a = 0.95


movingMean = do
  rawInput <- readFile "numbers.txt"
  let input = avg . clean $ rawInput
  print input


-- Calculating a Moving Median
strToInt raw = map (\s -> read s :: Int) (lines raw)
-- put smaller nums in maxheap and big nums in minheap
median (x:xs) maxheap minheap = case viewHead maxheap of
    Just theMax -> if x < theMax
            then median xs (insert x maxheap) minheap
            else median xs maxheap (insert x minheap)
    Nothing -> median xs (insert x maxheap) minheap

median [] maxheap minheap
    | size maxheap + 1 < size minheap =
        median [] (insert minelem maxheap) $ (snd.fromJust.view) minheap
    | size minheap + 1 < size maxheap =
        median [] ((snd.fromJust.view) maxheap) $ insert maxelem minheap
    |  size maxheap == size minheap =
        (fromIntegral maxelem + fromIntegral minelem) / 2.0
    | size maxheap > size minheap = fromIntegral maxelem
    | otherwise = fromIntegral minelem
    where
        maxelem = fromJust (viewHead maxheap)
        minelem = fromJust (viewHead minheap)

movingMedian = do
    raw <- readFile "numbers.txt"
    let input = strToInt raw
    print $ median input (empty :: MaxHeap Int) (empty :: MinHeap Int)


-- APPROXIMATING A LINEAR REGRESSION
linearR =do
    let xs = U.fromList [1.0, 2.0, 3.0, 4.0, 5.0] :: U.Vector Double
    let ys = U.fromList [1.0, 2.0, 1.3, 3.75, 2.25] :: U.Vector Double
    let (b,m) = linearRegression xs ys
    print $ concat ["y = ", show m, " x + ", show b]




--
main = do
        let values = [4,5,16,15,14,13,13,17]
        let values2 = [2,5,7,10,21, 18, 15]
        -- plot X11 $
        --    Data2D [ Title "Line Graph"
        --           , Style Linespoints
        --           , Color Blue]
        --    [] (zip [1..] values)

        -- plot X11 $ [ Data2D [Color Red] [] (zip [1..] values)
        --             , Data2D [Color Blue] [] (zip [1..] values2) ]

        -- 3D
        -- plot' [Interactive] X11
        --                [ Data3D [Color Red] [] [(1,1,1), (1,2,1), (0,1,1), (1,1,0)]
        --                , Data3D [Color Blue] [] [(4,3,2), (3,3,2), (3,2,3), (4,4,3)] ]


        -- regression

        let winEstimate = map (\x -> x * 0.425 + 0.7850000000000001) [1.3, 2.4 .. 4.7]
        let regressionLine = zip [1.3, 2.4 .. 4.7] winEstimate
        plot X11 [Data2D [Title "Runs Per Game VS Win % in 2014" ] [] (zip [1.0, 2.0, 3.0, 4.0, 5.0] [1.0, 2.0, 1.3, 3.75, 2.25]), Data2D [Title "Regression Line", Style Lines, Color Blue] [] regressionLine]
