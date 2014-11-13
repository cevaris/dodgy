module Dodgy.Objects.Sphere (drawSphere, drawStarSphere, drawAlienSphere) where 

import Control.Monad
import Data.Fixed
 
import Graphics.UI.GLUT

import Dodgy.GLUtils
import Dodgy.Types
import Dodgy.Objects.Types


spherePh:: Float -> [Float]
spherePh d = [ ph | ph <- [(-90.0)..90.0], ((mod' ph d) == 0 && ph < 90)]

sphereTh:: Float -> [Float]
sphereTh d = [th | th <- [0.0..360.0], (mod' th d) == 0]

drawLatBand :: Float -> (Float,Float) -> IO ()
drawLatBand d (ph, th) =  do

  let x = (-glSin(th))*glCos(ph)
      y =   glCos(th) *glCos(ph)
      z =              glSin(ph)

  drawNormal3f x y z
  drawTexCoord2f (th/360) (ph/180+0.5)
  drawVertex3f x y z

--Draw solid sphere
drawSphere :: State -> ObjectAttributes -> IO ()
drawSphere state object@(ObjectAttributes rotation scaleSize paint location noseVector upVector ambience4 diffuse4 specular4 emission4 shininess) = do

  preservingMatrix $ do
    preservingAttrib [AllServerAttributes] $ do
      let q = 5

      case (paint, location, scaleSize) of
        ((Just (Point4 px py pz pa)), (Just (lx, ly, lz)), (Just s))-> do 
          color3f px py pz
          translate $ vector3f lx ly lz
          scale3f s s s

          drawLightingEffects object

          forM_ (spherePh q) (\ph -> do
              renderPrimitive QuadStrip $ forM_ (sphereTh q) (\th -> do
                drawLatBand q (ph, th)
                drawLatBand q ((ph+5), th))
            ) 

          --mapM_ (\ph -> do
          --    renderPrimitive QuadStrip $ mapM_ (\th -> do
          --      drawLatBand q (ph, th)
          --      drawLatBand q ((ph+5), th)) (sphereTh q)
          --  ) (spherePh q)


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


drawAlienSphere :: State -> ObjectAttributes -> IO ()
drawAlienSphere state object@(ObjectAttributes rotation scaleSize paint location noseVector upVector ambience4 diffuse4 specular4 emission4 shininess) = do

  preservingMatrix $ do
    preservingAttrib [AllServerAttributes] $ do
      let q = 5
          tex = textures state
          alien' = alien tex

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
          textureBinding Texture2D $= Just alien'
          --textureWrapMode Texture2D S $= (Repeated, Clamp)
          --textureWrapMode Texture2D T $= (Repeated, Clamp)
          textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
          --textureFunction $= Modulate

          mapM_ (\ph -> do
              renderPrimitive QuadStrip $ mapM_ (\th -> do
                drawLatBand q (ph, th)
                drawLatBand q ((ph+5), th)) (sphereTh q)
            ) (spherePh q)