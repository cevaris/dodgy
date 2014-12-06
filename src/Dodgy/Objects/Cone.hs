module Dodgy.Objects.Cone (cone) where 

import Control.Monad

import Graphics.UI.GLUT

import Dodgy.GLUtils
import Dodgy.Types

cone :: Float -> Float -> IO ()
cone f b = do
  renderPrimitive Triangles $ do
    forM_ loop360 (\p -> do
                      drawVertex3f 0 0 f
                      --drawNormal3f f (glSin p) (glCos p)
                      --drawTexCoord2f f (glSin p)
                      drawVertex3f (glCos p) (glSin p) b
                       
                        
                      --drawNormal3f f (glSin p) (glCos p)
                      --drawTexCoord2f f (glSin p)
                      drawVertex3f (glCos (p+defd)) (glSin (p+defd)) b)
