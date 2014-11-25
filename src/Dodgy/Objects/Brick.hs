module Dodgy.Objects.Brick (drawBrick) where 
 
import Graphics.UI.GLUT
import Dodgy.GLUtils
import Dodgy.Types
import Dodgy.Objects.Types


drawBrick :: State -> Brick -> IO ()
drawBrick state brick = do

  let w      = 1.0
      tex    = textures state
      metal1' = metal1 tex
      brickType' = (kind brick)
      location' = Just (loc brick)
      paint' = (paint (attrs brick))
      scaleSize' = (scaleSize (attrs brick))


  preservingMatrix $ do
    preservingAttrib [AllServerAttributes] $ do

      drawLightingEffects (attrs brick)

      case (paint', location', scaleSize') of
        ((Just (Point4 px py pz pa)), (Just (lx, ly, lz)), (Just s)) -> do 
          color3f px py pz
          translate $ vector3f lx ly lz

          case brickType' of
            UnitBrick -> scale3f s s s
            WideBrick -> scale3f (3*s) s s
            LongBrick -> scale3f s s (3*s)

          texture Texture2D $= Enabled
          textureBinding Texture2D $= Just metal1'
          textureFilter Texture2D $= ((Nearest, Nothing), Nearest)

          renderPrimitive Quads $ do
            cube w

  
cube :: Float -> IO ()
cube w = do

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
