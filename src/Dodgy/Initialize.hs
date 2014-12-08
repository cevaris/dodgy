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

  --clearColor $= Color4 1 1 1 0
  clearColor $= Color4 (0/255) (0/255) (0/255) 0
  depthFunc $= Just Less

  --let fgc = Color4 0.5 0.5 0.5 1.0
  let fgc = Color4 1 1 1 1.0

  -- fog $= Enabled
  -- fogMode $= Exp 0.25
  -- --fogMode $= Exp2 0.35
  -- --fogIndex $= Index1 100
  -- --fogMode $= Linear 1 6
  -- fogColor $= fgc
  -- clearColor $= fgc

  
  displayCallback $= draw state
  reshapeCallback $= Just (reshape state)
  
  keyboardMouseCallback $= Just (keyboard state)
  visibilityCallback $= Just (visible state)

  mainLoop


makeState :: IO State
makeState = do
  f  <- newIORef 0
  t  <- newIORef 0
  ph <- newIORef 20 -- 20
  th <- newIORef 90 --(-30)
  gr <- newIORef 0
  zh <- newIORef 90
  fv <- newIORef 65 --65
  as <- newIORef 1
  di <- newIORef 2 --0.6 --3

  

  yl <- newIORef 0
  rl <- newIORef 3
  em <- newIORef 0
  df <- newIORef 65
  am <- newIORef 30 -- 30
  sp <- newIORef 85
  sm <- newIORef Smooth
  li <- newIORef True
  sh <- newIORef 5
  mv <- newIORef True

  zw <- newIORef 2

  mpPosX' <- newIORef 0
  mpPosY' <- newIORef 0
  coll'   <- newIORef makeFigherCollider
  score' <- newIORef 0
  lifep'  <- newIORef 100
  boost' <- newIORef 0
  zOffset' <- newIORef 0

  mode'   <- newIORef Medium

  level'  <- newIORef makeMapOne

  tx <- makeTextures

  i  <- newIORef ("","")
  return $ State {  
    frames = f, t0 = t, ph' = ph, th' = th, gr' = gr, zh' = zh, asp = as, fov = fv, dim = di, 
    ylight' = yl, rlight' = rl, emiss' = em, diff' = df, amb' = am, spec' = sp, smooth' = sm, light' = li, shine' = sh,
    move' = mv,
    zwall = zw,
    zOffset = zOffset',
    mpPosX = mpPosX', mpPosY = mpPosY',
    p_coll = coll',
    score = score',
    lifep  = lifep',
    boost = boost',
    mode  = mode',
    level = level',
    textures = tx,
    info = i
  }


makeTextures :: IO Textures
makeTextures = do
  steel' <- loadGLTextureFromFile "resources/textures/future-steel.jpg"
  comb'  <- loadGLTextureFromFile "resources/textures/comb-steel.jpg"
  -- water' <- loadGLTextureFromFile "resources/textures/water.jpg"
  -- borg'  <- loadGLTextureFromFile "resources/textures/borg.jpg"
  -- --alien' <- loadGLTextureFromFile "resources/textures/terran.jpg"
  -- alien' <- loadGLTextureFromFile "resources/textures/deathstar.png"
  star'  <- loadGLTextureFromFile "resources/textures/star.jpg"
  -- metal1' <- loadGLTextureFromFile "resources/textures/light-metal1.jpg"
  -- metal2' <- loadGLTextureFromFile "resources/textures/light-metal2.jpg"
  redTex' <- loadGLTextureFromFile "resources/textures/red.jpg"
  metal3' <- loadGLTextureFromFile "resources/textures/hull-steel.jpg"
  

  return $ Textures {
    steel = steel',
    comb  = comb',
    -- water = water',
    -- borg  = borg',
    -- alien = alien',
    star  = star',
    -- metal1 = metal1',
    -- metal2 = metal2',
    redBubbles = redTex',
    metal3 = metal3'
    
  }
