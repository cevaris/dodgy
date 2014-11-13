module Data.Random where

import System.Random

boundedPoints2f :: Float -> Float -> Int -> [(Float,Float)]
boundedPoints2f lower upper n = do 

  let t = take n 
      r = randomRs (lower,upper)
      x = t . r . mkStdGen $ n*10
      y = t . r . mkStdGen $ n*100

  zip x y

boundedXY3f :: Float -> Float -> Float -> Int -> [(Float, Float, Float)]
boundedXY3f lower upper z n = map (\(x,y) -> (x,y,z)) $ boundedPoints2f lower upper n