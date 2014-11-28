 module Dodgy.Collision where


import Graphics.UI.GLUT

import qualified  Dodgy.Objects.Types as C (Collider(..), CollisionState(Miss, Collision))
import Dodgy.GLUtils



testCollision :: Point3 -> C.Collider -> Point3 -> C.Collider -> C.CollisionState
testCollision (x1,y1,z1) c1 (x2,y2,z2) c2 = do
  let xAxis = (abs(x1 - x2) * 2) < ((C.c_width c1)  + (C.c_width c2))
      yAxis = (abs(y1 - y2) * 2) < ((C.c_height c1) + (C.c_height c2))
      zAxis = (abs(z1 - z2) * 2) < ((C.c_depth c1)  + (C.c_depth c2))
  if (xAxis && yAxis && zAxis)
    then C.Collision
    else C.Miss
