module Dodgy.Map where

-- import Graphics.UI.GLUT

-- import Dodgy.Objects.Types
-- import Dodgy.GLUtils

-- data Map 
--   = MapOne { brickMap :: [Brick]} 
--   | MapTwo { brickMap :: [Brick]} deriving (Show, Eq)


-- makeBrick :: Point3 -> BrickType -> Brick
-- makeBrick l k = Brick {
--   loc   = l,
--   kind  = k,
--   isDrawn = Enabled,
--   attrs = case k of
--            _ -> makeUnitBrickAttributes l
-- }

-- makeUnitBrickAttributes :: Point3 ->  ObjectAttributes
-- makeUnitBrickAttributes l = do
--   let ambience      = 1.0
--       diffusion     = 1.0
--       specularizion = 1.0
--       emission      = 1.0
--       shine         = 2
--       ambs  = (Point4 (0.01*ambience) (0.01*ambience) (0.01*ambience) 1.0)
--       diffs = (Point4 (0.01*diffusion) (0.01*diffusion) (0.01*diffusion) 1.0)
--       specs = (Point4 (0.01*specularizion) (0.01*specularizion) (0.01*specularizion) 1.0)
--       emiss = (Point4 0.0 0.0 (0.01*emission) 1.0)

--   ObjectAttributes {
--     rotation   = Nothing,
--     scaleSize  = Just 0.25,
--     paint      = Just darkGray,
--     location   = Just l,
--     noseVector = Just (0, 0, 1),
--     upVector   = Just (0,1,0),
--     ambience4  = Just darkGray,
--     diffuse4   = Just yellow,
--     specular4  = Just yellow,
--     emission4  = Just emiss,
--     shininess  = Just shine,
--     collider   = Nothing 
--  }

-- -- makeWideBrick :: Brick
-- -- makeLongBrick :: Brick

-- -- makeBricks :: [Brick]
-- -- makeBricks = do
-- --   let points = boundedXY3f (-1) 1 (-5) 5
-- --   (flip map) points (\l -> makeBrick l UnitBrick)



-- updateBrickMap :: [Brick] -> Map -> Map
-- updateBrickMap bm l = l { brickMap = bm }


-- -- makeMapOne :: Map
-- -- makeMapOne = MapOne {
-- --   brickMap = [
-- --       Brick { loc = (0,0,(-3)), kind = UnitBrick, isDrawn = Enabled },
-- --       Brick { loc = (0,1,(-4)), kind = UnitBrick, isDrawn = Enabled },
-- --       Brick { loc = (1,1,(-4)), kind = UnitBrick, isDrawn = Enabled }
-- --     ]
-- -- }
-- makeMapOne :: Map
-- makeMapOne = MapOne {
--   brickMap = [
--       makeBrick (0,0,(-3)) UnitBrick
--       makeBrick (0,1,(-4)) UnitBrick
--       makeBrick (1,1,(-4)) UnitBrick
--     ]
-- }


-- -- makeMapTwo :: Map
-- -- makeMapTwo = MapTwo {
-- --   brickMap = [
-- --       Brick { loc = (0,0,(-4)), kind = UnitBrick, isDrawn = Enabled },
-- --       Brick { loc = (0,4,(-4)), kind = UnitBrick, isDrawn = Enabled },
-- --       Brick { loc = (1,3,(-3)), kind = UnitBrick, isDrawn = Enabled }
-- --     ]
-- -- }
