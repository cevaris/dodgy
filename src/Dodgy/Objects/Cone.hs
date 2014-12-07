module Dodgy.Objects.Cone (cone) where 

import Control.Monad

import Graphics.UI.GLUT

import Dodgy.GLUtils
import Dodgy.Types

cone :: Float -> Float -> IO ()
cone f b = do
  renderPrimitive Triangles $ do
    forM_ loop360 (\p -> do
                      drawNormal3f 0 0f
                      drawTexCoord2f 0 f
                      drawVertex3f 0 0 f


                      drawNormal3f (glCos p) (glSin p) b
                      drawTexCoord2f (glSin p) b
                      drawVertex3f (glCos p) (glSin p) b
                       
                        
                      drawNormal3f (glCos (p+defd)) (glSin (p+defd)) b
                      drawTexCoord2f (glSin p) b
                      drawVertex3f (glCos (p+defd)) (glSin (p+defd)) b)
