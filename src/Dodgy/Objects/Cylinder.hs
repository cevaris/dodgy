module Dodgy.Objects.Cylinder (cylinder) where 

import Control.Monad

import Graphics.UI.GLUT

import Dodgy.GLUtils
import Dodgy.Types

cylinder :: Float -> Float -> IO ()
cylinder f b = do
  renderPrimitive QuadStrip $ do
    forM_ loop360 (\p -> do
                      drawNormal3f f (glSin p) (glCos p)
                      drawTexCoord2f f (glSin p)
                      drawVertex3f f (glSin p) (glCos p)
                       
                        
                      drawNormal3f b (glSin p) (glCos p)
                      drawTexCoord2f b (glSin p)
                      drawVertex3f b (glSin p) (glCos p))
