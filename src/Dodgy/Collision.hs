 module Dodgy.Collision where


import Graphics.UI.GLUT

import qualified  Dodgy.Objects.Types as C (Collider(..), CollisionState(Miss, Collision))
import Dodgy.GLUtils


calcPosition :: Point3 -> C.Collider -> C.Collider
calcPosition (x,y,z) c = C.BoxCollider {
  C.top    = y + (C.top c),
  C.bottom = y - (C.bottom c),
  C.left   = x - (C.left c),
  C.right  = x + (C.right c),
  C.front  = z + (C.front c),
  C.back   = z - (C.back c)
}

testCollision :: Point3 -> C.Collider -> Point3 -> C.Collider -> C.CollisionState
testCollision p1 c1 p2 c2 = executeCollisionTest ((calcPosition p1 c1), (calcPosition p2 c2))

executeCollisionTest :: (C.Collider, C.Collider) -> C.CollisionState
executeCollisionTest (c1, c2) = do
  let b = (C.bottom c1) < (C.top c2)
      t = (C.top c1)    > (C.bottom c2)
      l = (C.left c1)   > (C.right c2)
      r = (C.right c1)  < (C.left c2)
      f = (C.front c1)  < (C.back  c2)
      h = (C.back c1)   > (C.front c2)
  --if not (b || t || l || r || f || h)
  if not (f || h)
    then C.Collision
    else C.Miss

