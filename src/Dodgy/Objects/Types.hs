module Dodgy.Objects.Types where

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

data BrickType = WideBrick |  LongBrick | UnitBrick deriving (Show, Eq)

data Brick = Brick {
  loc      :: Point3,
  kind     :: BrickType
} deriving (Show, Eq)


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
  shininess  :: Shininess
} deriving (Show, Eq)