module Dodgy.Visibility (visible, idle) where

import Graphics.UI.GLUT


import Dodgy.GLUtils
import Dodgy.Types
import Dodgy.Idle


visible :: State -> Visibility -> IO ()
visible state Visible    = idleCallback $= Just (idle state)
visible _     NotVisible = idleCallback $= Nothing