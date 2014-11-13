module Dodgy.Idle where

import Data.Fixed

import Graphics.UI.GLUT

import Dodgy.GLUtils
import Dodgy.Types
import Dodgy.Objects.Types


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

  mpPosX' <- get (mpPosX state)
  mpPosY' <- get (mpPosY state)


  if mpPosX' > 1.0
    then mpPosX state $~! (\x -> (-0.99))
    else postRedisplay Nothing  

  if mpPosX' < (-1.0)
    then mpPosX state $~! (\x -> 0.99)
    else postRedisplay Nothing

  if mpPosY' > 1.0
    then mpPosY state $~! (\x -> (-0.99))
    else postRedisplay Nothing

  if mpPosY' < (-1.0)
    then mpPosY state $~! (\x -> 0.99)
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