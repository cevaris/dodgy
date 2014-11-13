module Graphics.Object.StarCluster (drawStarCluster) where 

import System.Random
import Control.Applicative
--import Control.Monad.State

 
import Graphics.UI.GLUT
import Graphics.UI.GLUT.Objects

import Graphics.Object.Sphere
import Dodgy.GLUtils
import Data.State

clusterPoints :: (RandomGen a, RandomGen b, RandomGen c) => Int -> a -> b -> c-> [(Float,Float,Float)]
clusterPoints n a b c = zip3 (take n (randomRs ((-1),1) a)) (take n (randomRs ((-1),1) b)) (take n (randomRs ((-1),1) c))
  
drawStarCluster :: State -> (Float, Float, Float) -> IO ()
drawStarCluster state (xT, yT, zT) = do

  let size = 0.02
      colorScale = 1.1

  mapM_ (\(x, y, z) -> do
    preservingMatrix $ do
      preservingAttrib [AllServerAttributes] $ do
        --color3f x y z
        translate $ vector3f (xT*x) (yT*y) (zT*z)
        scale3f (size*abs(x)) (size*abs(x)) (size*abs(x))

        drawSphere state $ ObjectAttributes {  
          rotation   = Nothing,
          scaleSize  = (Just 0.5),
          paint      = Just $ (Point4 (x*colorScale) (y*colorScale) (z*colorScale) 0),
          location   = (Just (0, 0, 0)),
          noseVector = Nothing,
          upVector   = Nothing,
          ambience4  = Nothing,
          diffuse4   = Nothing,
          specular4  = Nothing,
          emission4  = Nothing,
          shininess  = Nothing
        }
        postRedisplay Nothing
      ) (clusterPoints 10 (mkStdGen 1) (mkStdGen 10) (mkStdGen 30))
            
      