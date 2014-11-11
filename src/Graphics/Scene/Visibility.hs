module Graphics.Scene.Visibility (visible, idle) where

import Graphics.UI.GLUT

import Graphics.Util.GLUtils
import Data.State
import Graphics.Scene.Idle


visible :: State -> Visibility -> IO ()
visible state Visible    = idleCallback $= Just (idle state)
visible _     NotVisible = idleCallback $= Nothing