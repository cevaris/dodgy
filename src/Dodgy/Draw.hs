module Dodgy.Draw (draw) where 

import Graphics.UI.GLUT
import Graphics.GLUtil
import Graphics.UI.GLUT.Window

import Dodgy.Utils
import Dodgy.Map
import Dodgy.Types
import Dodgy.Objects.Types
import Dodgy.GLUtils
import Dodgy.HUD
import Dodgy.Objects.Grid
import Dodgy.Objects.Cube
import Dodgy.Objects.Fighter
import Dodgy.Objects.Sphere
import Dodgy.Objects.Brick
import Dodgy.Objects.Shuttle
import Dodgy.Objects.StarCubeMap
import Dodgy.Objects.Pyramid

draw :: State -> IO ()
draw state = do

  clear [ ColorBuffer, DepthBuffer ]

  f' <- get (frames state)
  t0' <- get (t0 state)
  ph <- get (ph' state)
  th <- get (th' state)
  gr <- get (gr' state)
  zh  <- get (zh' state)
  dim <- get (dim state)
  info <- get (info state)
  
  ylight <- get (ylight' state)
  rlight <- get (rlight' state)
  ambience <- get (amb' state)
  diffusion <- get (diff' state)
  specularizion <- get (spec' state)
  emission <- get (emiss' state)
  level <- get (level state)

  mpPosX <- get (mpPosX state)
  mpPosY <- get (mpPosY state)
  
  shineVal   <- get (shine' state)
  let shine = shineVal^2


  lightStatus <- get (light' state)
  shadeStatus <- get (smooth' state)
  c_state' <- get (c_state state)

  loadIdentity

  let ex = (-2)*dim*sin(toDeg(th))*cos(toDeg(ph))
      ey =    2*dim               *sin(toDeg(ph))
      ez =    2*dim*cos(toDeg(th))*cos(toDeg(ph))


  -- Shake the screen to notify collision
  case c_state' of
   Collision -> setLookAt (ex,ey,ez) (0,(sin(fromIntegral f')/2),(cos(fromIntegral f')/2)) (0,cos(toDeg(ph)),0)
   Miss -> setLookAt (ex,ey,ez) (0,0,0) (0,cos(toDeg(ph)),0)
    
  ------------------------------------

  let ambs     = (Point4 (0.01*ambience) (0.01*ambience) (0.01*ambience) 1.0)
      diffs    = (Point4 (0.01*diffusion) (0.01*diffusion) (0.01*diffusion) 1.0)
      specs    = (Point4 (0.01*specularizion) (0.01*specularizion) (0.01*specularizion) 1.0)
      loc3     = (rlight*glCos(zh), ylight, rlight*glSin(zh))
      loc4     = (Point4 (rlight*glCos(zh)) ylight (rlight*glSin(zh)) 1.0)
      yellow   = (Point4 1.0 1.0 0.0 1.0)
      white    = (Point4 1 1 1 1)
      black    = (Point4 0 0 0 1)
      lBlue    = (Point4 (173/255) (216/255) (230/255) 0)
      emiss    = (Point4 0.0 0.0 (0.01*emission) 1.0)
      darkGray = (Point4 (50/255) (50/255) (50/255) 0)
      snowGray = (Point4 (138/255) (138/255) (138/255) 0)


  -- Enable light
  normalize $= Enabled
  lighting $= Enabled
  lightModelLocalViewer $= Enabled
  colorMaterial $= Just (FrontAndBack, AmbientAndDiffuse)
  light (Light 0) $= Enabled

  shadeModel $= shadeStatus

  ambient4f ambs
  specular4f specs
  diffuse4f diffs
  position4f loc4

  -- Models
  drawShuttle state $ ObjectAttributes {
    rotation   = Nothing,
    scaleSize  = Just 1,
    paint      = Just white,
    location   = Just (0, mpPosY, mpPosX),
    noseVector = Just (1,0,0),
    upVector   = Just (0,1,0),
    ambience4  = Just white,
    diffuse4   = Just yellow,
    specular4  = Just yellow,
    emission4  = Nothing,
    shininess  = Just 1,
    collider   = Nothing
  }

  -- drawStarSphere state $ ObjectAttributes {
  --   rotation   = Nothing,
  --   scaleSize  = (Just 0.5),
  --   paint      = Just $ (Point4 255 255 0 0),
  --   location   = (Just loc3),
  --   noseVector = Nothing,
  --   upVector   = Nothing,
  --   ambience4  = Nothing,
  --   diffuse4   = Just yellow,
  --   specular4  = Just yellow,
  --   emission4  = Just yellow,
  --   shininess  = Just shine,
  --   collider   = Nothing
  -- }

  drawStarCubeMap state $ ObjectAttributes {
    rotation   = Nothing,
    scaleSize  = (Just 5),
    paint      = Just $ (Point4 255 255 0 0),
    location   = Just (0,0,0),
    noseVector = Nothing,
    upVector   = Nothing,
    ambience4  = Nothing,
    diffuse4   = Just snowBlue,
    specular4  = Just snowBlue,
    emission4  = Just snowBlue,
    shininess  = Just shine,
    collider   = Nothing
  }
 
  mapMR (brickMap level) (\brick -> case brick of 
      (Brick _ _ Disabled _) -> postRedisplay Nothing
      (Brick _ _ Enabled  _) -> drawBrick state brick)
      
     
  lighting $= Disabled
  ------------------------------------
  
  drawGrid 5

  preservingMatrix $ do
    preservingAttrib [AllServerAttributes] $ do
      -- color3f 0 0 0
      color3f 1 1 1
      glWindowPos 5 30
      renderString Helvetica18 $ (fst info)
      glWindowPos 5 5
      renderString Helvetica18 $ (snd info)

  swapBuffers
  updateInfo state
  reportErrors
