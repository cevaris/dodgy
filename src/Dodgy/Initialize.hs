module Dodgy.Initialize where

import Control.Monad
import Data.IORef ( IORef, newIORef )

import Graphics.UI.GLUT

import Dodgy.Random
import Dodgy.GLUtils
import Dodgy.Textures
import Dodgy.Types
import Dodgy.Objects.Types
import Dodgy.Map

import Dodgy.Draw
import Dodgy.Input
import Dodgy.Reshape
import Dodgy.Visibility
import Dodgy.Timers


initDodgy :: [String] -> IO ()
initDodgy args = do
  initialDisplayMode $= [ RGBMode, WithDepthBuffer, DoubleBuffered ]

  size <- get screenSize
  case size of
    (Size x y) -> initialWindowSize $= Size (x `div` 2) y

  initialWindowPosition $= Position 3 3
  
  _window <- createWindow "Dodgy - Adam Cardenas"

  state <- makeState
  clearColor $= Color4 1 1 1 0
  --clearColor $= Color4 (0/255) (0/255) (0/255) 0
  depthFunc $= Just Less

  displayCallback $= draw state
  reshapeCallback $= Just (reshape state)
  
  keyboardMouseCallback $= Just (keyboard state)
  visibilityCallback $= Just (visible state)

  mainLoop


makeState :: IO State
makeState = do
  f  <- newIORef 0
  t  <- newIORef 0
  ph <- newIORef 5 -- 20
  th <- newIORef 0 --(-30)
  gr <- newIORef 0
  zh <- newIORef 90
  fv <- newIORef 130 --65
  as <- newIORef 1
  di <- newIORef 5
  --di <- newIORef 0.5
  
  yl <- newIORef 0
  rl <- newIORef 5
  em <- newIORef 0
  df <- newIORef 65
  am <- newIORef 30
  sp <- newIORef 85
  sm <- newIORef Smooth
  li <- newIORef True
  sh <- newIORef 5
  mv <- newIORef True

  zw <- newIORef 4.0

  mpPosX' <- newIORef 0
  mpPosY' <- newIORef 0
  coll'   <- newIORef makeUnitCollider

  mode'   <- newIORef Medium

  level'  <- newIORef makeMapOne

  tx <- makeTextures

  i  <- newIORef ("","")
  return $ State {  
    frames = f, t0 = t, ph' = ph, th' = th, gr' = gr, zh' = zh, asp = as, fov = fv, dim = di, 
    ylight' = yl, rlight' = rl, emiss' = em, diff' = df, amb' = am, spec' = sp, smooth' = sm, light' = li, shine' = sh,
    move' = mv,
    zwall = zw,
    mpPosX = mpPosX', mpPosY = mpPosY',
    s_coll = coll',
    mode  = mode',
    level = level',
    textures = tx,
    info = i
  }


makeTextures :: IO Textures
makeTextures = do
  steel' <- loadGLTextureFromFile "resources/textures/future-steel.jpg"
  comb'  <- loadGLTextureFromFile "resources/textures/comb-steel.jpg"
  water' <- loadGLTextureFromFile "resources/textures/water.jpg"
  borg'  <- loadGLTextureFromFile "resources/textures/borg.jpg"
  --alien' <- loadGLTextureFromFile "resources/textures/terran.jpg"
  alien' <- loadGLTextureFromFile "resources/textures/deathstar.png"
  star'  <- loadGLTextureFromFile "resources/textures/star.jpg"
  metal1' <- loadGLTextureFromFile "resources/textures/metal1.jpg"
  metal2' <- loadGLTextureFromFile "resources/textures/metal2.jpg"

  return $ Textures {
    steel = steel',
    comb  = comb',
    water = water',
    borg  = borg',
    alien = alien',
    star  = star',
    metal1 = metal1',
    metal2 = metal2'
  }
