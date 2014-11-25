module Dodgy.Collision where


import Graphics.UI.GLUT

-- import qualified  Dodgy.Objects.Types as C (Collider(bottom, top, left, right, front, back), CollisionState(Miss, Collision))
import qualified  Dodgy.Objects.Types as C (Collider(..), CollisionState(Miss, Collision))

 
checkCollide :: C.Collider -> C.Collider -> C.CollisionState
checkCollide c1 c2 = do
  let b = (C.bottom c1) < (C.top c2)
      t = (C.top c1)    > (C.bottom c2)
      l = (C.left c1)   > (C.right c2)
      r = (C.right c1)  < (C.left c2)
      f = (C.front c1)  < (C.back  c2)
      h = (C.back c1)   > (C.front c2)
  if not b || t || l || r || f || h
    then C.Collision
    else C.Miss
      
