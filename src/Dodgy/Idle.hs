module Dodgy.Idle where

import Data.IORef
import Data.Fixed

import Graphics.UI.GLUT

import Dodgy.GLUtils
import Dodgy.Types
import Dodgy.Objects.Types
import Dodgy.Map
import Dodgy.Collision

idle :: State -> IdleCallback
idle state = do

  ph  <- get (ph' state)
  th  <- get (th' state)
  gr  <- get (gr' state)
  zh  <- get (zh' state)
  dim' <- get (dim state)
  fov' <- get (fov state)

  spec <- get (spec' state)
  amb <- get (amb' state)
  diff <- get (diff' state)
  shine <- get (shine' state)
  emiss <- get (emiss' state)
  lightStatus <- get (light' state)
  moveStatus <- get (move' state)

  f <- get (frames state)
  lv <- get (level state)

  mpPosX' <- get (mpPosX state)
  mpPosY' <- get (mpPosY state)
  c1      <- get (s_coll state)
  -- putStrLn $ show f
  --putStrLn $ show (brickMap level)


  let brickMap'  = updateBrickLocations (brickMap lv) f
      brickMap'' = map (updateIsDrawn 10.0) brickMap'
      level'     = updateBrickMap brickMap'' lv

  let p1 = (mpPosX', mpPosY', 2.75)
      tcoll = (\b -> do
                  let coll2 = (collider $ attrs b)
                      p2    = (loc b)
                 

                  case coll2 of
                    Nothing   -> "MISS"
                    (Just c2) -> do
                      -- let c1' = calcPosition p1 c1
                      --     c2' = calcPosition p2 c2
                          -- b = (bottom c1') < (top c2')
                          -- t = (top c1')    > (bottom c2')
                          -- l = (left c1')   > (right c2')
                          -- r = (right c1')  < (left c2')
                          -- f = (front c1')  < (back  c2')
                          -- ba = (back c1')  > (front c2')
                          -- snap = [b, t, l, r, f, ba]
                      --show c1' ++ " " ++ show c2' ++ " " ++ show snap ++ " " ++ show (not (foldr1 (||) snap)) ++ " " ++ show (testCollision p1 c1 p2 c2))
                      -- show p1 ++ " " ++ show c1' ++ " " ++ show p2 ++ " " ++ show c2' ++ " " ++ (show $ testCollision p1 c1 p2 c2))
                      show $ testCollision p1 c1 p2 c2)

              
                  -- case coll2 of
                  --   Nothing   -> Miss
                  --   (Just c2) -> do
                  --     testCollision p1 c1 p2 c2)

  mapM_ (putStrLn . tcoll) brickMap''
  --let collisions = map tcoll brickMap''
  --putStrLn $ show collisions ++ "\n\n-----"
  
  -- putStrLn $ show $ map (\(Brick l k d) -> show l ++ " " ++ show d) brickMap''
      
  level state $~! (\x -> level')

  let maxMove = 5.0
      epsilon = 0.01
      swapPos = maxMove - epsilon
      
  if mpPosX' > maxMove
    then mpPosX state $~! (\x -> (-swapPos))
    else postRedisplay Nothing  

  if mpPosX' < (-maxMove)
    then mpPosX state $~! (\x -> swapPos)
    else postRedisplay Nothing

  if mpPosY' > maxMove
    then mpPosY state $~! (\x -> (-swapPos))
    else postRedisplay Nothing

  if mpPosY' < (-maxMove)
    then mpPosY state $~! (\x -> swapPos)
    else postRedisplay Nothing

  ---------------------------------------

  
  if moveStatus
    then do 
      t <- get elapsedTime
      let seconds = ((fromIntegral t))/1000.0
      zh' state $~! (\x -> mod' (90*seconds) 360)
    else postRedisplay Nothing

  --if fov' < 55
  --  then fov state $~! (\x -> 55)
  --  else postRedisplay Nothing  

  --if fov' < 55
  --  then fov state $~! (\x -> 55)
  --  else postRedisplay Nothing

  --if dim' < 1
  --  then dim state $~! (\x -> 1)
  --  else postRedisplay Nothing

  --if gr > 360
  --  then gr' state $~! (\x -> 0)
  --  else gr' state $~! (+2)
  
  --if ((-360) > ph || ph > 360)
  --  then ph' state $~! (\x -> 0)
  --  else postRedisplay Nothing

  --if ((-360) > th || th > 360)
  --  then th' state $~! (\x -> 0)
  --  else postRedisplay Nothing

  --if spec > 100
  --  then spec' state $~! (\x -> 100)
  --  else if spec < 0
  --    then spec' state $~! (\x -> 0)
  --    else postRedisplay Nothing

  --if diff > 100
  --  then diff' state $~! (\x -> 100)
  --  else if diff < 0
  --    then diff' state $~! (\x -> 0)
  --    else postRedisplay Nothing

  --if amb > 100
  --  then amb' state $~! (\x -> 100)
  --  else if amb < 0
  --    then amb' state $~! (\x -> 0)
  --    else postRedisplay Nothing

  --if shine > 100
  --  then shine' state $~! (\x -> 100)
  --  else if shine < 0
  --    then shine' state $~! (\x -> 0)
  --    else postRedisplay Nothing

  --if emiss > 100
  --  then emiss' state $~! (\x -> 100)
  --  else if emiss < 0
  --    then emiss' state $~! (\x -> 0)
  --    else postRedisplay Nothing


  postRedisplay Nothing
