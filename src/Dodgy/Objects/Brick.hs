module Dodgy.Objects.Brick (drawBrick) where 
 
import Graphics.UI.GLUT
import Dodgy.GLUtils
import Dodgy.Types
import Dodgy.Textures
import Dodgy.Objects.Types
import Dodgy.Objects.Cube
import Dodgy.Objects.Pyramid


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
drawBrick state brick@(Brick _ SpecialBrick _ _) = compPyramid state brick



compPyramid :: State -> Brick -> IO ()
compPyramid state brick = do
  
  let w      = 0.5/2
      sep    = 1.0/8
      tex    = (textures state)
      brickKind = (kind brick)
      location' = Just (loc brick)
      paint' = (paint (attrs brick))
      scaleSize' = (scaleSize (attrs brick))


  preservingMatrix $ do
    preservingAttrib [AllServerAttributes] $ do

      drawLightingEffects (attrs brick)
      
      texture Texture2D $= Enabled
      textureBinding Texture2D $= Just (steel tex)
      textureFilter Texture2D $= ((Nearest, Nothing), Nearest)      

      case (paint', location') of
        ((Just (Point4 px py pz pa)), (Just (lx, ly, lz))) -> do 
          color3f px py pz
          translate $ vector3f lx (ly+sep) lz

      octahedronTop w

  preservingMatrix $ do
    preservingAttrib [AllServerAttributes] $ do

      drawLightingEffects (attrs brick)
      
      texture Texture2D $= Enabled
      textureBinding Texture2D $= Just (steel tex)
      textureFilter Texture2D $= ((Nearest, Nothing), Nearest)      

      case (paint', location') of
        ((Just (Point4 px py pz pa)), (Just (lx, ly, lz))) -> do 
          color3f px py pz
          translate $ vector3f lx (ly-sep) lz
          rotate1f 180 $ vector3f 1 0 0

      octahedronTop w
  
     



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

      texture Texture2D $= Enabled
      textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
      textureBinding Texture2D $= Just (bindBrickTexture tex brickKind)

      drawLightingEffects (attrs brick)

      case (paint', location', scaleSize') of
        ((Just (Point4 px py pz pa)), (Just (lx, ly, lz)), (Just s)) -> do 
          color3f px py pz
          translate $ vector3f lx ly lz

          case brickKind of
            UnitBrick -> scale3f s s s
            WideBrick -> scale3f s s (3*s)
            LongBrick -> scale3f (3*s) s s
            _         -> postRedisplay Nothing

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

        blend $= Enabled
        blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
        
        color4f (Point4 (204/255) 0 0 0.4)
  
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
      translate $ vector3f lx ly (lz-sep)
      cube w
  -- Right
  preservingMatrix $ do
    preservingAttrib [AllServerAttributes] $ do
      colorCube
      translate $ vector3f lx ly (lz+sep)
      cube w
