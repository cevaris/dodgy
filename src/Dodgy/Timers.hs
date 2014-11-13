module Dodgy.Timers (timer) where

import Graphics.UI.GLUT

import Dodgy.Types
import Dodgy.GLUtils

timerFrequencyMillis :: Timeout
timerFrequencyMillis = 20

timer :: State -> TimerCallback
timer state = do
  addTimerCallback timerFrequencyMillis (timer state)