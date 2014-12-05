module Dodgy.Objects.Shuttle (drawShuttle) where 

import Control.Monad
import Data.Fixed
import GHC.Float

import Graphics.UI.GLUT

import Dodgy.GLUtils
import Dodgy.Types
import Dodgy.Objects.Types
import Dodgy.Objects.Sphere

drawShuttle :: State -> ObjectAttributes -> IO ()
drawShuttle state object@(ObjectAttributes rotation scaleSize paint location noseVector upVector _ _ _ _ _ _) = do
  

  case (location, noseVector, upVector, scaleSize, paint) of
    ((Just (lx, ly, lz)), (Just (dx, dy, dz)), (Just (ux, uy, uz)), (Just s), (Just (Point4 cx cy cz ca))) -> do 

    let wid  = 0.05
        nose = 0.50
        cone = 0.20
        wing = 0.00
        strk = (-0.20)
        tail = (-0.50)
        d0 = sqrt(dx*dx+dy*dy+dz*dz)
        x0 = dx/d0
        y0 = dy/d0
        z0 = dz/d0
        --  Unit vector in "up" direction
        d1 = sqrt(ux*ux+uy*uy+uz*uz)
        x1 = ux/d1
        y1 = uy/d1
        z1 = uz/d1
        -- Cross product gives the third vector
        x2 = y0*z1-y1*z0
        y2 = z0*x1-z1*x0
        z2 = x0*y1-x1*y0
        tex = textures state
        steel' = steel tex
        comb' = comb tex
        defd = 5
        loop360 = [ p | p <- [0..360], (mod' p defd) == 0]

    mat <- newMatrix RowMajor $ listf [x0, x1,  x2, 0,
                                       y0, y1,  y2, 0,
                                       z0, z1,  z2, 0,
                                       0,  0,   0,  1]


    drawLightingEffects object


    -- Capsule Cylindar
    preservingMatrix $ do
      preservingAttrib [AllServerAttributes] $ do
        
        color3f cx cy cz
        translate $ vector3f lx ly lz
        let ns = s/14
        scale3f ns ns s
        multMatrix (mat :: GLmatrix GLfloat)

        texture Texture2D $= Enabled
        textureBinding Texture2D $= Just steel'
        textureFilter Texture2D $= ((Nearest, Nothing), Nearest)

        renderPrimitive QuadStrip $ do
          forM_ loop360 (\p -> do
                            
                            drawNormal3f cone (glSin p) (glCos p)
                            drawTexCoord2f cone (glSin p)
                            drawVertex3f cone (glSin p) (glCos p)
                            
                            
                            drawNormal3f tail (glSin p) (glCos p)
                            drawTexCoord2f tail (glSin p)
                            drawVertex3f tail (glSin p) (glCos p))

    -- Capsule Tail Cap
    preservingMatrix $ do
      preservingAttrib [AllServerAttributes] $ do
        
        -- Offset, scale and rotate
        color3f cx cy cz
        
        translate $ vector3f lx ly ((lz-tail)*s)
        let ns = s/14
        scale3f ns ns (ns/3)
        multMatrix (mat :: GLmatrix GLfloat)

        texture Texture2D $= Enabled
        textureBinding Texture2D $= Just steel'
        textureFilter Texture2D $= ((Nearest, Nothing), Nearest)

        sphere

    -- Cockpit Back
    preservingMatrix $ do
      preservingAttrib [AllServerAttributes] $ do

        translate $ vector3f lx ly ((lz-cone)*s)
        let ns = s/14
        scale3f ns ns (ns/3)
        multMatrix (mat :: GLmatrix GLfloat)

        color4f snowGray
        sphere

    -- Cockpit Floor
    preservingMatrix $ do
      preservingAttrib [AllServerAttributes] $ do

        color3f cx cy cz
        translate $ vector3f lx ((lz+0.03)*s) ((lz-cone)*s)
        let ns = s/14
        scale3f (ns/1.2) (ns/6) (ns*2.6)
        multMatrix (mat :: GLmatrix GLfloat)

        color4f snowGray
        sphere
  
    -- Tip sphere
    preservingMatrix $ do
      preservingAttrib [AllServerAttributes] $ do
        
        color3f cx cy cz
        translate $ vector3f lx ((ly+0.025)*s) ((lz-cone)*s)
        let ns = s/16
            zs = ns*3
        scale3f ns ns zs
        multMatrix (mat :: GLmatrix GLfloat)

        blend $= Enabled
        blendFunc $= (SrcAlpha, OneMinusSrcAlpha)

        color4f (Point4 0 0 0 0.4)
        sphere

    
    -- Tip sphere
    preservingMatrix $ do
      preservingAttrib [AllServerAttributes] $ do
        
        -- Offset, scale and rotate
        color3f cx cy cz
        translate $ vector3f lx ly ((lz-cone)*s)
        let ns = s/14
            zs = ns*3
        scale3f ns ns zs
        multMatrix (mat :: GLmatrix GLfloat)

        texture Texture2D $= Enabled
        textureBinding Texture2D $= Just steel'
        textureFilter Texture2D $= ((Nearest, Nothing), Nearest)

        sphere
          

    -- Wings
    preservingMatrix $ do
      preservingAttrib [AllServerAttributes] $ do
        
        -- Offset, scale and rotate
        color3f cx cy cz
        translate $ vector3f lx ly lz
        scale3f s s s
        multMatrix (mat :: GLmatrix GLfloat)

        texture Texture2D $= Enabled
        textureBinding Texture2D $= Just steel'
        textureFilter Texture2D $= ((Nearest, Nothing), Nearest)

        -- Top of Side Wings
        renderPrimitive Triangles $ do

          -- Right Top Wing
          drawNormal3f 0 1 0
          drawTexCoord2f 1 1
          drawVertex3f wing 0.0001  wid
          drawTexCoord2f 1 0
          drawVertex3f tail 0.0001  wid
          drawTexCoord2f 0 0
          drawVertex3f tail 0.0001  0.5

          -- Left Top Wing
          drawNormal3f 0 1 0
          drawTexCoord2f 1 1
          drawVertex3f wing 0.0001 (-wid)
          drawTexCoord2f 1 0
          drawVertex3f tail 0.0001 (-wid)
          drawTexCoord2f 0 0
          drawVertex3f tail 0.0001 (-0.5)

        --color3f 1 0 0
        --color3f cx cy cz
        --color3f (211/255) (211/255) (211/255)

        -- Top Fin Wing
        renderPrimitive Triangles $ do
          drawNormal3f 0 0 1
          drawTexCoord2f 1 0
          drawVertex3f strk 0.0 0.0001
          drawTexCoord2f 0 1
          drawVertex3f tail 0.3 0.0001
          drawTexCoord2f 0 0
          drawVertex3f tail 0.0 0.0001

          drawNormal3f 0 0 (-1)
          drawTexCoord2f 1 0
          drawVertex3f strk 0.0 (-0.0001)
          drawTexCoord2f 0 1
          drawVertex3f tail 0.3 (-0.0001)
          drawTexCoord2f 0 0
          drawVertex3f tail 0.0 (-0.0001)




        texture Texture2D $= Enabled
        textureBinding Texture2D $= Just comb'
        textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
        textureWrapMode Texture2D S $= (Repeated, Repeat)
        textureWrapMode Texture2D T $= (Repeated, Repeat)

        -- Bottom of Side Wings
        renderPrimitive Triangles $ do


          -- Right Bottom Wing
          drawNormal3f 0 (-1) 0
          drawTexCoord2f 1 1
          drawVertex3f wing (-0.0001)  wid
          drawTexCoord2f 1 0
          drawVertex3f tail (-0.0001)  wid
          drawTexCoord2f 0 0
          drawVertex3f tail (-0.0001)  0.5


          -- Left Bottom Wing
          drawNormal3f 0 (-1) 0
          drawTexCoord2f 1 1
          drawVertex3f wing (-0.0001) (-wid)
          drawTexCoord2f 1 0
          drawVertex3f tail (-0.0001) (-wid)
          drawTexCoord2f 0 0
          drawVertex3f tail (-0.0001) (-0.5)

        --    -- Bottom Cone
        --   drawNormal3f 1 (-cone/wid) 0
        --   drawTexCoord2f 1 0.5
        --   drawVertex3f nose  0.0  0.0
        --   drawTexCoord2f 0 1 
        --   drawVertex3f cone (-wid) (wid)
        --   drawTexCoord2f 0 0
        --   drawVertex3f cone (-wid) (-wid)


        -- renderPrimitive Quads $ do
        --   -- Bottom Capsule
        --   drawNormal3f 0 (-1) 0
        --   drawTexCoord2f 0 0
        --   drawVertex3f cone (-wid)  wid
        --   drawTexCoord2f 1 0
        --   drawVertex3f cone (-wid) (-wid)
        --   drawTexCoord2f 1 1
        --   drawVertex3f tail (-wid) (-wid)
        --   drawTexCoord2f 0 1
        --   drawVertex3f tail (-wid)  wid
      
        
      