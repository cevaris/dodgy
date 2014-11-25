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

import Dodgy.GLUtils


data BrickType   = WideBrick |  LongBrick | UnitBrick deriving (Show, Eq)

data Map 
  = MapOne { brickMap :: [Brick]} 
  | MapTwo { brickMap :: [Brick]} deriving (Show, Eq)


makeBrick :: Point3 -> BrickType -> Brick
makeBrick l k = Brick {
  loc   = l,
  kind  = k,
  isDrawn = Enabled,
  attrs = case k of
           _ -> makeUnitBrickAttributes l
}

makeUnitBrickAttributes :: Point3 ->  ObjectAttributes
makeUnitBrickAttributes l = do
  let ambience      = 1.0
      diffusion     = 1.0
      specularizion = 1.0
      emission      = 1.0
      shine         = 2
      ambs  = (Point4 (0.01*ambience) (0.01*ambience) (0.01*ambience) 1.0)
      diffs = (Point4 (0.01*diffusion) (0.01*diffusion) (0.01*diffusion) 1.0)
      specs = (Point4 (0.01*specularizion) (0.01*specularizion) (0.01*specularizion) 1.0)
      emiss = (Point4 0.0 0.0 (0.01*emission) 1.0)

  ObjectAttributes {
    rotation   = Nothing,
    scaleSize  = Just 0.25,
    paint      = Just darkGray,
    location   = Just l,
    noseVector = Just (0, 0, 1),
    upVector   = Just (0,1,0),
    ambience4  = Just darkGray,
    diffuse4   = Just yellow,
    specular4  = Just yellow,
    emission4  = Just emiss,
    shininess  = Just shine,
    collider   = Nothing 
 }

-- makeWideBrick :: Brick
-- makeLongBrick :: Brick

-- makeBricks :: [Brick]
-- makeBricks = do
--   let points = boundedXY3f (-1) 1 (-5) 5
--   (flip map) points (\l -> makeBrick l UnitBrick)



updateBrickMap :: [Brick] -> Map -> Map
updateBrickMap bm l = l { brickMap = bm }


-- makeMapOne :: Map
-- makeMapOne = MapOne {
--   brickMap = [
--       Brick { loc = (0,0,(-3)), kind = UnitBrick, isDrawn = Enabled },
--       Brick { loc = (0,1,(-4)), kind = UnitBrick, isDrawn = Enabled },
--       Brick { loc = (1,1,(-4)), kind = UnitBrick, isDrawn = Enabled }
--     ]
-- }
makeMapOne :: Map
makeMapOne = MapOne {
  brickMap = [
      makeBrick (0,0,(-3)) UnitBrick,
      makeBrick (0,1,(-4)) WideBrick,
      makeBrick (1,1,(-4)) LongBrick
    ]
}


-- makeMapTwo :: Map
-- makeMapTwo = MapTwo {
--   brickMap = [
--       Brick { loc = (0,0,(-4)), kind = UnitBrick, isDrawn = Enabled },
--       Brick { loc = (0,4,(-4)), kind = UnitBrick, isDrawn = Enabled },
--       Brick { loc = (1,3,(-3)), kind = UnitBrick, isDrawn = Enabled }
--     ]
-- }


data Brick = Brick {
  loc      :: Point3,
  kind     :: BrickType,
  isDrawn  :: Capability,
  attrs    :: ObjectAttributes
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
updateIsDrawn zw b@(Brick (x, y, z) _ _ _)
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

drawLightingEffects :: ObjectAttributes -> IO ()
drawLightingEffects object@(ObjectAttributes rotation scaleSize paint location noseVector upVector ambience4 diffuse4 specular4 emission4 shininess collider) = do
  
  case shininess of 
      (Just sh) -> do 
        materialShininess FrontAndBack $= (iToGL sh)
      _ -> postRedisplay Nothing

  case specular4 of 
    (Just point4) -> do 
      materialSpecular FrontAndBack $= pointToColor4f point4
    _ -> postRedisplay Nothing

  case diffuse4 of 
    (Just point4) -> do 
      materialDiffuse FrontAndBack $= pointToColor4f point4
    _ -> postRedisplay Nothing

  case ambience4 of 
    (Just point4) -> do 
      materialAmbient FrontAndBack $= pointToColor4f point4
    _ -> postRedisplay Nothing

  case emission4 of 
    (Just point4) -> do 
      materialEmission FrontAndBack $= pointToColor4f point4
    _ -> postRedisplay Nothing
