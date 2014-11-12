module Data.State where
import Data.IORef ( IORef, newIORef )

import Graphics.UI.GLUT
import Graphics.GLUtil

import Graphics.Util.Textures

----------------------------------------------------------------------------------------------------------------
-- Global State
data ChangeDirection = Increase | Decrease deriving (Show)

data Difficulty = Hard | Medium | Easy deriving (Show, Eq)

data ProjectionView = PerspectiveView | OrthogonalView | FirstPersonView deriving (Show, Eq)

data Direction = UpDirection | DownDirection | LeftDirection | RightDirection deriving (Show, Eq)

data State = State {
  frames  :: IORef Int,
  t0      :: IORef Int,
  ph'     :: IORef Float,
  th'     :: IORef Float,
  gr'     :: IORef Float,
  zh'     :: IORef Float,
  asp     :: IORef Float,
  fov     :: IORef Float,
  dim     :: IORef Float,
   
  ylight' :: IORef Float,
  rlight' :: IORef Float,
  emiss'  :: IORef Float,
  diff'   :: IORef Float,
  amb'    :: IORef Float,
  spec'   :: IORef Float,
  smooth' :: IORef ShadingModel,
  light'  :: IORef Bool,
  shine'  :: IORef Int,
  move'   :: IORef Bool,

  mpPos   :: IORef Float,
  mode    :: IORef Difficulty,

  textures :: Textures,
   
  info    :: IORef (String,String)
}

makeState :: IO State
makeState = do
  f  <- newIORef 0
  t  <- newIORef 0
  ph <- newIORef 20 -- 20
  th <- newIORef 0 --(-30)
  gr <- newIORef 0
  zh <- newIORef 90
  fv <- newIORef 100 --65
  as <- newIORef 1
  di <- newIORef 2
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

  mpPos' <- newIORef 0
  mode'  <- newIORef Medium

  tx <- makeTextures

  i  <- newIORef ("","")
  return $ State {  
    frames = f, t0 = t, ph' = ph, th' = th, gr' = gr, zh' = zh, asp = as, fov = fv, dim = di, 
    ylight' = yl, rlight' = rl, emiss' = em, diff' = df, amb' = am, spec' = sp, smooth' = sm, light' = li, shine' = sh,
    move' = mv,
    mpPos = mpPos',
    mode  = mode',
    textures = tx,
    info = i
  }


data Textures = Textures {
  steel :: TextureObject,
  comb :: TextureObject,
  water :: TextureObject,
  borg  :: TextureObject,
  alien :: TextureObject,
  star  :: TextureObject,
  metal1 :: TextureObject,
  metal2 :: TextureObject
} deriving (Show, Eq)

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


type Point3 = (Float, Float, Float)
data Point4 = Point4 Float Float Float Float deriving (Show, Eq)

type RotAngle   = Maybe Float
type Scale      = Maybe Float
type Paint      = Maybe Point4
type Location   = Maybe Point3
type NoseVector = Maybe Point3
type UpVector   = Maybe Point3
type Ambience4  = Maybe Point4
type Diffuse4   = Maybe Point4
type Specular4  = Maybe Point4
type Emission4  = Maybe Point4
type Shininess  = Maybe Int



--type ObjectAttributes = (Scale, Paint, Location, NoseVector, UpVector, Ambience4, Diffuse4, Specular4, Shininess)
data ObjectAttributes = ObjectAttributes {
  rotation   :: RotAngle,
  scaleSize  :: Scale,
  paint      :: Paint,
  location   :: Location,
  noseVector :: NoseVector,
  upVector   :: UpVector,
  ambience4  :: Ambience4,
  diffuse4   :: Diffuse4,
  specular4  :: Specular4,
  emission4  :: Emission4,
  shininess  :: Shininess
} deriving (Show, Eq)
