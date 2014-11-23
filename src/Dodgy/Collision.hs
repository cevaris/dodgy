module Dodgy.Collision where


import Graphics.UI.GLUT

import qualified  Dodgy.Objects.Types as C (Collider, CollisionState(Miss, Collision))
 
didCollide :: C.Collider -> C.Collider -> C.CollisionState
didCollide c1 c2 = C.Miss
