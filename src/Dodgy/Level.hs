module Dodgy.Map where

import Dodgy.Objects.Types

data Map 
  = MapOne { brickMap :: [Brick]} 
  | MapTwo { brickMap :: [Brick]} deriving (Show, Eq)



makeMapOne :: Map
makeMapOne = MapOne {
  brickMap = [
      Brick { loc = (0,0,(-3)), kind = UnitBrick },
      Brick { loc = (0,1,(-4)), kind = UnitBrick },
      Brick { loc = (1,1,(-4)), kind = UnitBrick }
    ]
}


makeMapTwo :: Map
makeMapTwo = MapTwo {
  brickMap = [
      Brick { loc = (0,0,(-4)), kind = UnitBrick },
      Brick { loc = (0,4,(-4)), kind = UnitBrick },
      Brick { loc = (1,3,(-3)), kind = UnitBrick }
    ]
}