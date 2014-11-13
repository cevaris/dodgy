module Graphics.Scene.Timers (timer) where

import Graphics.UI.GLUT

import Dodgy.GLUtils
import Data.State

timerFrequencyMillis :: Timeout
timerFrequencyMillis = 20

timer :: State -> TimerCallback
timer state = do
  addTimerCallback timerFrequencyMillis (timer state)