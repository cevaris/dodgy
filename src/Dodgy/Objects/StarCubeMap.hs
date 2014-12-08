module Dodgy.Objects.StarCubeMap (drawStarCubeMap) where 
 
import Graphics.UI.GLUT

import Dodgy.GLUtils
import Dodgy.Types
import Dodgy.Textures
import Dodgy.Objects.Types
import Dodgy.Objects.Cube

drawStarCubeMap :: State -> ObjectAttributes -> IO ()
drawStarCubeMap state object@(ObjectAttributes rotation scaleSize paint location noseVector upVector _ _ _ _ _ _) = do

  preservingMatrix $ do
    preservingAttrib [AllServerAttributes] $ do

      drawLightingEffects object

      case (paint, location, scaleSize) of
        ((Just (Point4 px py pz pa)), (Just (lx, ly, lz)), (Just s)) -> do 
          color3f px py pz
          translate $ vector3f lx ly lz
          scale3f s s s

      let base = "resources/textures/"
          w    = 1
      
      nebLeft <- loadGLTextureFromFile (base ++ "nutron-left.png")
      nebRight <- loadGLTextureFromFile (base ++ "nutron-right.png")
      nebBack <- loadGLTextureFromFile (base ++ "nutron-back.png")
      nebFront <- loadGLTextureFromFile (base ++ "nutron-front.png")
      nebTop <- loadGLTextureFromFile (base ++ "nutron-top.png")
      nebBottom <- loadGLTextureFromFile (base ++ "nutron-bottom.png")

      
      let bindTex = (\t -> do
                        texture Texture2D $= Enabled
                        textureBinding Texture2D $= Just t
                        textureFilter Texture2D $= ((Nearest, Nothing), Nearest))

      bindTex nebFront
      renderPrimitive Quads $ do        
        -- Front
        drawNormal3f 0 0 w
        drawTexCoord2f 0 0
        drawVertex3f (-w) (-w)  w
        drawTexCoord2f 1 0
        drawVertex3f w (-w)  w
        drawTexCoord2f 1 1
        drawVertex3f w w  w
        drawTexCoord2f 0 1
        drawVertex3f (-w) w  w
        
      bindTex nebBack
      renderPrimitive Quads $ do
        -- Back
        drawNormal3f 0  0 (-w)
        drawTexCoord2f 0 0
        drawVertex3f w (-w) (-w)
        drawTexCoord2f 1 0
        drawVertex3f (-w) (-w) (-w)
        drawTexCoord2f 1 1
        drawVertex3f (-w) w (-w)
        drawTexCoord2f 0 1
        drawVertex3f w w (-w)

      bindTex nebRight
      renderPrimitive Quads $ do
        -- Right
        drawNormal3f w  0  0
        drawTexCoord2f 0 0
        drawVertex3f w (-w) w
        drawTexCoord2f 1 0
        drawVertex3f w (-w) (-w)
        drawTexCoord2f 1 1
        drawVertex3f w w (-w)
        drawTexCoord2f 0 1
        drawVertex3f w w w

      bindTex nebLeft
      renderPrimitive Quads $ do
        -- Left
        drawNormal3f (-w)  0  0
        drawTexCoord2f 0 0
        drawVertex3f (-w) (-w) (-w)
        drawTexCoord2f 1 0
        drawVertex3f (-w) (-w) w
        drawTexCoord2f 1 1
        drawVertex3f (-w) w w
        drawTexCoord2f 0 1
        drawVertex3f (-w) w (-w)

      bindTex nebTop
      renderPrimitive Quads $ do
        -- Top
        drawNormal3f 0 w  0
        drawTexCoord2f 0 0
        drawVertex3f (-w) w w
        drawTexCoord2f 1 0
        drawVertex3f w w w
        drawTexCoord2f 1 1
        drawVertex3f w w (-w)
        drawTexCoord2f 0 1
        drawVertex3f (-w) w (-w)

      bindTex nebBottom
      renderPrimitive Quads $ do
        -- Bottom
        drawNormal3f 0 (-1) 0
        drawTexCoord2f 0 0
        drawVertex3f (-w) (-w) (-w)
        drawTexCoord2f 1 0
        drawVertex3f w (-w) (-w)
        drawTexCoord2f 1 1
        drawVertex3f w (-w) w
        drawTexCoord2f 0 1
        drawVertex3f (-w) (-w) w
  
      
      
