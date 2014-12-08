module Dodgy.Objects.Types where

import Graphics.UI.GLUT

import Dodgy.GLUtils


data BrickType = WideBrick |  LongBrick | UnitBrick | HealthBrick | SpecialBrick deriving (Show, Eq)

data Map 
  = MapOne { brickMap :: [Brick]} 
  | MapTwo { brickMap :: [Brick]} deriving (Show, Eq)


makeBrick :: Point3 -> BrickType -> Brick
makeBrick l k = Brick {
  loc   = l,
  kind  = k,
  isDrawn = Enabled,
  attrs = makeBrickAttributes l k
}

makeBrickAttributes :: Point3 -> BrickType -> ObjectAttributes
makeBrickAttributes l k = do
  let ambience      = 30
      diffusion     = 65
      specularizion = 85
      emission      = 0
      shine         = 5^2
      ambs  = (Point4 (0.01*ambience) (0.01*ambience) (0.01*ambience) 1.0)
      diffs = (Point4 (0.01*diffusion) (0.01*diffusion) (0.01*diffusion) 1.0)
      specs = (Point4 (0.01*specularizion) (0.01*specularizion) (0.01*specularizion) 1.0)
      emiss = (Point4 0.0 0.0 (0.01*emission) 1.0)

  let collider' = case k of
        UnitBrick -> makeUnitCollider
        WideBrick -> makeWideCollider
        LongBrick -> makeLongCollider
        HealthBrick -> makeHpCollider
        SpecialBrick -> makeSpCollider
        
  ObjectAttributes {
    rotation   = Nothing,
    scaleSize  = Just 1.0,
    paint      = Just snowGray,
    location   = Nothing, -- Do not use this location
    noseVector = Just (0, 0, 1),
    upVector   = Just (0,1,0),
    ambience4  = Just snowGray,
    diffuse4   = Just yellow,
    specular4  = Just yellow,
    emission4  = Just emiss,
    shininess  = Just shine,
    collider   = Just collider'
 }

updateBrickMap :: [Brick] -> Map -> Map
updateBrickMap bm l = l { brickMap = bm }

makeFigherCollider = BoxCollider {
  c_width  = 1,
  c_height = 1/2,
  c_depth  = 1
}

makeUnitCollider = BoxCollider {
  c_width  = 1/2,
  c_height = 1/2,
  c_depth  = 1/2
}
makeWideCollider = BoxCollider {
  c_width  = 3,
  c_height = 1/2,
  c_depth  = 1/2
}
makeLongCollider = BoxCollider {
  c_width  = 1/2,
  c_height = 1/2,
  c_depth  = 3
}
makeHpCollider = BoxCollider {
  c_width  = 6/10,
  c_height = 6/10,
  c_depth  = 6/10
}
makeSpCollider = BoxCollider {
  c_width  = 1,
  c_height = 1,
  c_depth  = 1
}

makeMapOne :: Map
makeMapOne = MapOne {
  brickMap = [
     makeBrick (3,0,0) SpecialBrick,
     makeBrick (5,(-1),0) WideBrick,
     makeBrick (7,0,0) HealthBrick,
     makeBrick (10,1,0) LongBrick,
     makeBrick (1,0,0) UnitBrick,
     makeBrick (16,1,0) WideBrick,     
     makeBrick (17,1,(-1)) LongBrick,
     makeBrick (18,2,1) LongBrick
     ]
}




data Brick = Brick {
  loc      :: Point3,
  kind     :: BrickType,
  isDrawn  :: Capability,
  attrs    :: ObjectAttributes
} deriving (Show, Eq)

-- Speed of moving objects
speed :: Float
speed = 0.01

updateBrickLoc :: Float -> Brick -> Brick
updateBrickLoc i b = do
  let (x,y,z) = loc b
      loc'    =  x-(speed*i)
  
  b { loc = (loc', y, z) }

updateBrickIsDrawn :: Capability -> Brick -> Brick
updateBrickIsDrawn c b = b { isDrawn = c }

updateBrickLocations :: [Brick] -> Float -> [Brick]
updateBrickLocations bricks interval = do
  let update = updateBrickLoc interval
  map (update) bricks

updateIsDrawn :: Float -> Brick -> Brick
updateIsDrawn zw b@(Brick (x, y, z) _ _ _)
  | z > zw    = b { isDrawn = Disabled }
  | otherwise = b

-- exeCollision :: (Point3, Collider) -> [Brick] -> CollisionState
-- exeCollision (p1, c1) bricks = do
--   let collisions = (flip map) bricks (\b -> do
--                                          let coll2 = (collider $ attrs b)

--                                          case coll2 of
--                                            Nothing   -> Miss
--                                            (Just c2) -> testCollision p1 c1 (loc b) c2)
--   Miss






data CollisionState = Collision | Miss deriving (Show, Eq)
data Collider = BoxCollider {
  c_width :: Float,
  c_height :: Float,
  c_depth :: Float
} deriving(Show, Eq)






data Textures = Textures {
  steel :: TextureObject,
  comb :: TextureObject,
  -- water :: TextureObject,
  -- borg  :: TextureObject,
  -- alien :: TextureObject,
  star  :: TextureObject,
  -- metal1 :: TextureObject,
  -- metal2 :: TextureObject,
  metal3 :: TextureObject,
  redBubbles :: TextureObject
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
