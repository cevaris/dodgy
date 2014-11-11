module Graphics.Object.StarSphere (drawStarSphere) where 
 
import Graphics.UI.GLUT

import Graphics.Util.GLUtils
import Data.State

drawStarSphere :: State -> ObjectAttributes -> IO ()
drawStarSphere state object@(ObjectAttributes rotation scaleSize paint location noseVector upVector ambience4 diffuse4 specular4 emission4 shininess) = do

  preservingMatrix $ do
    preservingAttrib [AllServerAttributes] $ do
      let q = 5
          tex = textures state
          star' = star tex

      case (paint, location, scaleSize) of
        ((Just (Point4 px py pz pa)), (Just (lx, ly, lz)), (Just s))-> do 
          color3f px py pz
          translate $ vector3f lx ly lz
          scale3f s s s
          
          case (rotation) of
            ((Just a)) -> do
              rotate1f a $ vector3f 0 1 0
            _ -> postRedisplay Nothing

          rotate1f 90 $ vector3f 1 0 0

          drawLightingEffects object

          texture Texture2D $= Enabled
          textureBinding Texture2D $= Just star'
          textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
          textureFunction $= Modulate

          mapM_ (\ph -> do
              renderPrimitive QuadStrip $ mapM_ (\th -> do
                drawLatBand q (ph, th)
                drawLatBand q ((ph+5), th)) (sphereTh q)
            ) (spherePh q)