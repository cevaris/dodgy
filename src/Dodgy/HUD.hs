module Dodgy.HUD (updateInfo) where 

import Control.Monad ( when )


import Graphics.UI.GLUT
--import Graphics.GLUtil
--import Graphics.UI.GLUT.Window


import Dodgy.Types
import Dodgy.Objects.Types

----------------------------------------------------------------------------------------------------------------
-- Debug info
updateInfo :: State -> IO ()
updateInfo state = do 
  frames state $~! (+1)
  t0' <- get (t0 state)
  t <- get elapsedTime
  when (t - t0' >= 1000) $ do
    f <- get (frames state)
    ph <- get (ph' state)
    th <- get (th' state)
    gr <- get (gr' state)
    zh <- get (zh' state)
    asp <- get (asp state)
    fov <- get (fov state)
    dim <- get (dim state)
    mpPosX <- get (mpPosX state)
    mpPosY <- get (mpPosY state)
    mode <- get (mode state)
    
    spec <- get (spec' state)
    amb <- get (amb' state)
    diff <- get (diff' state)
    shine <- get (shine' state)
    emiss <- get (emiss' state)
    lightStatus <- get (light' state)
    shadStatus <- get (smooth' state)

    hp <- get (lifep state)
    sc <- get (score state)
    zo <- get (zOffset state)
    bo <- get (boost state)

    let seconds = fromIntegral (t - t0') / 1000 :: GLfloat
        fps = fromIntegral f / seconds
        result = ("Life Points: " ++ show hp ++ " Boost: " ++ show bo ++ "", "Score: " ++ show sc ++ " zOffset: " ++ show zo ++ "[mpPosX " ++ show mpPosX ++ "] [mpPosY " ++ show mpPosY ++ "]")
        --result = ("[mpPosX " ++ show mpPosX ++ "] [mpPosY " ++ show mpPosY ++ "] [mode "++ show mode ++ "]",
        --         "")
        --result = ("[ph " ++ round2 ph ++ "] [th " ++ round2 th ++ "] [zh " ++ round2 zh ++ "] [zoom " ++ show dim ++ "] [lightStatus " ++ show lightStatus ++  "] [shading " ++ show shadStatus ++  "] ",
        --          "[specular " ++ show spec ++  "] [ambience " ++ show amb ++  "] [diffuse " ++ show diff ++  "] [shininess " ++ show shine ++  "] [emission " ++ show emiss ++  "] ")
    info state $= result
    t0 state $= t
    frames state $= 0
