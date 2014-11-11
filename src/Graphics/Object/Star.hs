module Graphics.Object.Star (drawStar) where 
 
import Graphics.UI.GLUT
import Graphics.UI.GLUT.Objects

import Graphics.Util.GLUtils

import Graphics.Object.Sphere
import Data.State

-- Draw solid pyramid
--  scale (s)
--  at (x,y,z)
--drawStar :: Float-> (Float, Float, Float) -> IO ()
--drawStar s (x, y, z) = do
drawStar :: State -> ObjectAttributes -> IO ()
drawStar state object@(ObjectAttributes rotation scaleSize paint location noseVector upVector ambience4 diffuse4 specular4 emission4 shininess) = do
  postRedisplay Nothing
  --preservingMatrix $ do
  --  preservingAttrib [AllServerAttributes] $ do  

  --    case (paint, location, scaleSize) of
  --      ((Just (px, py, pz)), (Just (lx, ly, lz)), (Just s))-> do 
  --        color3f px py pz
  --        translate $ vector3f lx ly lz
  --        scale3f s s s
          
  --        --drawSphere s 0.5 (0,0,0)
  --        drawSphere object
  --      _ -> putStrLn $ "Start Light case Fail: " ++ (show object)