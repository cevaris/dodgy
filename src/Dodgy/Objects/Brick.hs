module Dodgy.Objects.Brick (drawBrick) where 
 
import Graphics.UI.GLUT
import Dodgy.GLUtils
import Dodgy.Types
import Dodgy.Objects.Types
import Dodgy.Objects.Cube


bindBrickTexture :: Textures -> BrickType -> TextureObject
bindBrickTexture tex WideBrick = metal3 tex
bindBrickTexture tex LongBrick = metal3 tex
bindBrickTexture tex UnitBrick = metal3 tex
bindBrickTexture tex HealthBrick = redBubbles tex
bindBrickTexture tex _         = comb tex

drawBrick :: State -> Brick -> IO ()
drawBrick state brick@(Brick _ UnitBrick _ _) = cuboid state brick
drawBrick state brick@(Brick _ LongBrick _ _) = cuboid state brick
drawBrick state brick@(Brick _ WideBrick _ _) = cuboid state brick
drawBrick state brick@(Brick _ HealthBrick _ _) = plus state brick

cuboid :: State -> Brick -> IO ()
cuboid state brick = do
  
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
      
      cube w

plus :: State -> Brick -> IO ()
plus state brick = do
  
  let w      = 0.5/4
      sep    = 1.0/4
      tex    = (textures state)
      brickKind = (kind brick)
      (lx, ly, lz) = (loc brick)
      paint' = (paint (attrs brick))
      scaleSize' = (scaleSize (attrs brick))


  
  let colorCube = do
        drawLightingEffects (attrs brick)
        case (paint') of        
         ((Just (Point4 px py pz pa))) -> do
           color3f px py pz
        
        texture Texture2D $= Enabled
        textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
        textureBinding Texture2D $= Just (bindBrickTexture tex brickKind)
  
  -- Center
  preservingMatrix $ do
    preservingAttrib [AllServerAttributes] $ do
      colorCube
      translate $ vector3f lx ly lz
      cube w
  -- Top
  preservingMatrix $ do
    preservingAttrib [AllServerAttributes] $ do
      colorCube
      translate $ vector3f lx (ly+sep) lz
      cube w
  -- Bottom
  preservingMatrix $ do
    preservingAttrib [AllServerAttributes] $ do
      colorCube
      translate $ vector3f lx (ly-sep) lz
      cube w
  -- Left 
  preservingMatrix $ do
    preservingAttrib [AllServerAttributes] $ do
      colorCube
      translate $ vector3f (lx-sep) ly lz
      cube w
  -- Right
  preservingMatrix $ do
    preservingAttrib [AllServerAttributes] $ do
      colorCube
      translate $ vector3f (lx+sep )ly lz
      cube w
