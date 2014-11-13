import Control.Monad ( when )
import Control.Applicative

import Graphics.UI.GLUT
import Graphics.GLUtil
import Graphics.UI.GLUT.Window

import Data.State
import Binding.Input
import Dodgy.GLUtils
import Graphics.Scene.Reshape
import Graphics.Scene.Visibility
import Graphics.Scene.Timers

import Graphics.Object.Grid
import Graphics.Object.Cube
import Graphics.Object.SteelFighter
import Graphics.Object.AlienSphere
import Graphics.Object.StarSphere

----------------------------------------------------------------------------------------------------------------
-- Debug info
updateInfo :: State -> IO ()
updateInfo state = do 
  frames state $~! (+1)
  t0' <- get (t0 state)
  t <- get elapsedTime
  when (t - t0' >= 1000) $ do
    f <- get (frames state)
    ph <- get (ph' state)
    th <- get (th' state)
    gr <- get (gr' state)
    zh <- get (zh' state)
    asp <- get (asp state)
    fov <- get (fov state)
    dim <- get (dim state)
    mpPosX <- get (mpPosX state)
    mpPosY <- get (mpPosY state)
    mode <- get (mode state)
    
    spec <- get (spec' state)
    amb <- get (amb' state)
    diff <- get (diff' state)
    shine <- get (shine' state)
    emiss <- get (emiss' state)
    lightStatus <- get (light' state)
    shadStatus <- get (smooth' state)

    let seconds = fromIntegral (t - t0') / 1000 :: GLfloat
        fps = fromIntegral f / seconds
        result = ("[mpPosX " ++ show mpPosX ++ "] [mpPosY " ++ show mpPosY ++ "] [mode "++ show mode ++ "]",
                  "")
        --result = ("[ph " ++ round2 ph ++ "] [th " ++ round2 th ++ "] [zh " ++ round2 zh ++ "] [zoom " ++ show dim ++ "] [lightStatus " ++ show lightStatus ++  "] [shading " ++ show shadStatus ++  "] ",
        --          "[specular " ++ show spec ++  "] [ambience " ++ show amb ++  "] [diffuse " ++ show diff ++  "] [shininess " ++ show shine ++  "] [emission " ++ show emiss ++  "] ")
    info state $= result
    t0 state $= t
    frames state $= 0


draw :: State -> IO ()
draw state = do

  clear [ ColorBuffer, DepthBuffer ]

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

  mpPosX <- get (mpPosX state)
  mpPosY <- get (mpPosY state)
  
  shineVal   <- get (shine' state)
  let shine = shineVal^2


  lightStatus <- get (light' state)
  shadeStatus <- get (smooth' state)


  loadIdentity

  let ex = (-2)*dim*sin(toDeg(th))*cos(toDeg(ph))
      ey =    2*dim               *sin(toDeg(ph))
      ez =    2*dim*cos(toDeg(th))*cos(toDeg(ph))
  setLookAt (ex,ey,ez) (0,0,0) (0,cos(toDeg(ph)),0)



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
      
  drawSteelFighter state $ ObjectAttributes {
    rotation   = Nothing,
    scaleSize  = Just 2,
    paint      = Just white,
    location   = Just (0, 1, 1),
    noseVector = Just (0, (-1), 1),
    upVector   = Just (0,1,0),
    ambience4  = Just white,
    diffuse4   = Just yellow,
    specular4  = Just yellow,
    emission4  = Just emiss,
    shininess  = Just shine
  }

  drawCube state $ ObjectAttributes {
    rotation   = Nothing,
    scaleSize  = Just 0.25,
    paint      = Just white,
    location   = Just (mpPosX, mpPosY, 2.75),
    noseVector = Just (0, 0, 1),
    upVector   = Just (0,1,0),
    ambience4  = Just white,
    diffuse4   = Just yellow,
    specular4  = Just yellow,
    emission4  = Just emiss,
    shininess  = Just shine
  }

  drawStarSphere state $ ObjectAttributes {
    rotation   = Nothing,
    scaleSize  = (Just 0.25),
    paint      = Just $ (Point4 255 255 0 0),
    location   = (Just loc3),
    noseVector = Nothing,
    upVector   = Nothing,
    ambience4  = Nothing,
    diffuse4   = Just yellow,
    specular4  = Just yellow,
    emission4  = Just yellow,
    shininess  = Just shine
  }

  --let bricks = replicate 5 $ ObjectAttributes {
  --    rotation   = Nothing,
  --    scaleSize  = Just 0.25,
  --    paint      = Just white,
  --    location   = Just (0, 0, (-2)),
  --    noseVector = Just (1, 0, 0),
  --    upVector   = Just (0,1,0),
  --    ambience4  = Just white,
  --    diffuse4   = Just yellow,
  --    specular4  = Just yellow,
  --    emission4  = Just emiss,
  --    shininess  = Just shine
  --  }
  --mapM_ (putStrLn . show) bricks

  lighting $= Disabled
  ------------------------------------
  
  drawGrid 5

  preservingMatrix $ do
    preservingAttrib [AllServerAttributes] $ do
      color3f 0 0 0
      glWindowPos 5 30
      renderString Helvetica18 $ (fst info)
      glWindowPos 5 5
      renderString Helvetica18 $ (snd info)

  swapBuffers
  updateInfo state
  reportErrors


myInit :: [String] -> State -> IO ()
myInit args state = do
  clearColor $= Color4 1 1 1 0
  --clearColor $= Color4 (0/255) (0/255) (0/255) 0
  depthFunc $= Just Less  


main :: IO ()
main = do

    (_progName, args) <- getArgsAndInitialize
    initialDisplayMode $= [ RGBMode, WithDepthBuffer, DoubleBuffered ]

    size <- get screenSize
    case size of
      (Size x y) -> initialWindowSize $= Size (x `div` 2) y
    
    _window <- createWindow "Dodgy - Adam Cardenas"

    

    state <- makeState
    myInit args state

    displayCallback $= draw state
    reshapeCallback $= Just (reshape state)
    
    keyboardMouseCallback $= Just (keyboard state)
    --keyboardUpCallback $= Just (keyboard state)
    visibilityCallback $= Just (visible state)

    mainLoop
  


