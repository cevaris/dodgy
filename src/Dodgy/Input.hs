module Dodgy.Input where

import System.Exit ( exitWith, ExitCode(ExitSuccess), exitFailure )

import Graphics.UI.GLUT

import Dodgy.GLUtils
import Dodgy.Types
import Dodgy.Objects.Types

----------------------------------------------------------------------------------------------------------------
-- Key Binding
keyboard :: State -> KeyboardMouseCallback
keyboard state (SpecialKey KeyUp)   _ _ _ = modRotateSpecial state KeyUp
keyboard state (SpecialKey KeyDown) _ _ _ = modRotateSpecial state KeyDown
keyboard state (SpecialKey KeyLeft) _ _ _ = modRotateSpecial state KeyLeft
keyboard state (SpecialKey KeyRight)_ _ _ = modRotateSpecial state KeyRight

keyboard state (Char 'z')           Up _ _ = modDim state Decrease
keyboard state (Char 'Z')           Up _ _ = modDim state Increase

keyboard state (Char 'a')           _ _ _ = modMainPlayer state LeftDirection
keyboard state (Char 'd')           _ _ _ = modMainPlayer state RightDirection
keyboard state (Char 'w')           _ _ _ = modMainPlayer state UpDirection
keyboard state (Char 's')           _ _ _ = modMainPlayer state DownDirection

keyboard state (Char 'j')           _ _ _ = modRotate state LeftDirection
keyboard state (Char 'l')           _ _ _ = modRotate state RightDirection
keyboard state (Char 'i')           _ _ _ = modRotate state UpDirection
keyboard state (Char 'k')           _ _ _ = modRotate state DownDirection

keyboard state (Char ' ')           _ _ _ = modBoost state

--keyboard state (Char '[')           Up _ _ = modLightHeight state Decrease
--keyboard state (Char ']')           Up _ _ = modLightHeight state Increase

--keyboard state (Char 'r')           Up _ _ = modLightRadius state Decrease
--keyboard state (Char 'R')           Up _ _ = modLightRadius state Increase

--keyboard state (Char 's')           Up _ _ = modSpecular state Decrease
--keyboard state (Char 'S')           Up _ _ = modSpecular state Increase

--keyboard state (Char 'd')           Up _ _ = modDiffusion state Decrease
--keyboard state (Char 'D')           Up _ _ = modDiffusion state Increase

--keyboard state (Char 'a')           Up _ _ = modAmbience state Decrease
--keyboard state (Char 'A')           Up _ _ = modAmbience state Increase

--keyboard state (Char 'n')           Up _ _ = modShininess state Decrease
--keyboard state (Char 'N')           Up _ _ = modShininess state Increase

--keyboard state (Char 'e')           Up _ _ = modEmission state Decrease
--keyboard state (Char 'E')           Up _ _ = modEmission state Increase

--keyboard state (Char 'm')           keyState _ _ = toggleLightMovement state keyState
--keyboard state (Char 'M')           keyState _ _ = toggleLightMovement state keyState

--keyboard state (Char 'l')           keyState _ _ = toggleLight state keyState
--keyboard state (Char 'L')           keyState _ _ = toggleLight state keyState

--keyboard state (Char 'h')           keyState _ _ = toggleShading state keyState
--keyboard state (Char 'H')           keyState _ _ = toggleShading state keyState

keyboard _     (Char '\27')         _ _ _ = exitWith ExitSuccess
keyboard _     _                    _ _ _ = return ()


modRotateSpecial :: State -> SpecialKey -> IO ()
modRotateSpecial state KeyDown  = ph' state $~! (\x -> x - 5)
modRotateSpecial state KeyUp    = ph' state $~! (+5)
modRotateSpecial state KeyRight = th' state $~! (\x -> x - 5)
modRotateSpecial state KeyLeft  = th' state $~! (+5)

modRotate :: State -> Direction -> IO ()
modRotate state DownDirection  = ph' state $~! (\x -> x - 5)
modRotate state UpDirection    =  ph' state $~! (+5)
modRotate state RightDirection = th' state $~! (\x -> x - 5)
modRotate state LeftDirection  =  th' state $~! (+5)

modBoost :: State -> IO ()
modBoost  state  = do
  boost state $~! (\x -> 20)

playerDelta = 0.1
modMainPlayer :: State -> Direction -> IO ()
modMainPlayer state LeftDirection  = do
  mpPosX state $~! (subtract playerDelta)
modMainPlayer state RightDirection = do
  mpPosX state $~! (+playerDelta) 
modMainPlayer state DownDirection  = do
  mpPosY state $~! (subtract playerDelta)
modMainPlayer state UpDirection    = do
  mpPosY state $~! (+playerDelta) 

toggleLightMovement :: State -> KeyState -> IO ()
toggleLightMovement state  Up = do
  moveStatus <- get (move' state)
  if moveStatus
    then move' state $~! (\x -> False)
    else move' state $~! (\x -> True)
toggleLightMovement state Down = postRedisplay Nothing


toggleShading :: State -> KeyState -> IO ()
toggleShading state  Up = do
  smoothStatus <- get (smooth' state)
  if smoothStatus == Smooth
    then smooth' state $~! (\x -> Flat)
    else smooth' state $~! (\x -> Smooth)
toggleShading state Down = postRedisplay Nothing


toggleLight :: State -> KeyState -> IO ()
toggleLight state  Up = do
  lightStatus <- get (light' state)
  if lightStatus
    then light' state $~! (\x -> False)
    else light' state $~! (\x -> True)
toggleLight state Down = postRedisplay Nothing


-- modEmission :: State -> ChangeDirection -> IO ()
-- modEmission state Decrease = do
--   emiss' state $~! (\x -> x - 5)
-- modEmission state Increase  = do
--   emiss' state $~! (+5)  


-- modShininess :: State -> ChangeDirection -> IO ()
-- modShininess state Decrease = do
--   shine' state $~! (\x -> x - 1)
-- modShininess state Increase  = do
--   shine' state $~! (+1)  

-- modAmbience :: State -> ChangeDirection -> IO ()
-- modAmbience state Decrease = do
--   amb' state $~! (\x -> x - 5)
-- modAmbience state Increase  = do
--   amb' state $~! (+5)

-- modDiffusion :: State -> ChangeDirection -> IO ()
-- modDiffusion state Decrease = do
--   diff' state $~! (\x -> x - 5)
-- modDiffusion state Increase  = do
--   diff' state $~! (+5)

-- modSpecular :: State -> ChangeDirection -> IO ()
-- modSpecular state Decrease = do
--   spec' state $~! (\x -> x - 5)
-- modSpecular state Increase  = do
--   spec' state $~! (+5)

-- modLightRadius :: State -> ChangeDirection -> IO ()
-- modLightRadius state Decrease = do
--   rlight' state $~! (\x -> x - 1)
-- modLightRadius state Increase  = do
--   rlight' state $~! (+1)

-- modLightHeight :: State -> ChangeDirection -> IO ()
-- modLightHeight state Decrease = do
--   ylight' state $~! (\x -> x - 0.1)
-- modLightHeight state Increase  = do
--   ylight' state $~! (+0.1)
  
modFov :: State -> ChangeDirection -> IO ()
modFov state Decrease = do
  fov state $~! (\x -> x - 2)
  postRedisplay Nothing
modFov state Increase = do
  fov state $~! (+2)
  postRedisplay Nothing  

modDim :: State -> ChangeDirection -> IO ()
modDim state Decrease = do
  dim state $~! (\x -> x - 0.1)
  postRedisplay Nothing
modDim state Increase = do
  dim state $~! (+0.1)
  postRedisplay Nothing  
