module Dodgy.Objects.Brick (drawBrick) where 
 
import Graphics.UI.GLUT
import Dodgy.GLUtils
import Dodgy.Types
import Dodgy.Objects.Types
import Dodgy.Objects.Cube


bindBrickTexture :: Textures -> BrickType -> TextureObject
bindBrickTexture tex WideBrick = steel tex
bindBrickTexture tex LongBrick = metal2 tex
bindBrickTexture tex UnitBrick = comb tex

bindBrickTexture tex _         = comb tex

drawBrick :: State -> Brick -> IO ()
drawBrick state brick = do

  let w      = 0.5
      tex    = (textures state)
      brickKind = (kind brick)
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

          case brickKind of
            UnitBrick -> scale3f s s s
            WideBrick -> scale3f (3*s) s s
            LongBrick -> scale3f s s (3*s)
            _         -> postRedisplay Nothing

      texture Texture2D $= Enabled
      textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
      textureBinding Texture2D $= Just (bindBrickTexture tex brickKind)
      
      let cuboid = cube w

      case brickKind of
         UnitBrick -> cuboid
         WideBrick -> cuboid
         LongBrick -> cuboid

                    
  
