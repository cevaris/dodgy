module Dodgy.Objects.Types where
-- ( Brick,
--   loc,
  
--   BrickType,
--   Point3,
--   Point4,
--   Collider,
--   CollisionState,
--   Textures,
--   ObjectAttributes
--   ) where

import Graphics.UI.GLUT

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

type Point3 = (Float, Float, Float)
data Point4 = Point4 Float Float Float Float deriving (Show, Eq)

data BrickType   = WideBrick |  LongBrick | UnitBrick deriving (Show, Eq)

data Brick = Brick {
  loc      :: Point3,
  kind     :: BrickType,
  isDrawn  :: Capability
} deriving (Show, Eq)

-- Speed of moving objects
speed :: Float
speed = 0.0075

updateBrickLoc :: Int -> Brick -> Brick
updateBrickLoc i b = do
  let (x,y,z) = loc b
      -- loc'    = ((fromIntegral i)::Float) * speed + z
      loc'    = ((fromIntegral (i `mod` 5))::Float) * speed + z      
  
  b { loc = (x, y, loc' ) }

updateBrickIsDrawn :: Capability -> Brick -> Brick
updateBrickIsDrawn c b = b { isDrawn = c }

updateBrickLocations :: [Brick] -> Int -> [Brick]
updateBrickLocations bricks interval = do
  let update = updateBrickLoc interval
  map (update) bricks

updateIsDrawn :: Float -> Brick -> Brick
updateIsDrawn zw b@(Brick (x, y, z) _ _)
  | z > zw    = b { isDrawn = Disabled }
  | otherwise = b

data CollisionState = Collision | Miss deriving (Show, Eq)
data Collider = BoxCollider {
  top :: Float,
  bottom :: Float,
  left :: Float,
  right :: Float,
  front :: Float,
  back :: Float
} deriving(Show, Eq)

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
  shininess  :: Shininess,
  collider   :: Maybe Collider
} deriving (Show, Eq)
