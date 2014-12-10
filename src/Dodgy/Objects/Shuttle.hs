module Dodgy.Objects.Shuttle (drawShuttle) where 

import Control.Monad
import Data.Fixed
import GHC.Float

import Graphics.UI.GLUT

import Dodgy.GLUtils
import Dodgy.Types
import Dodgy.Objects.Types
import Dodgy.Objects.Sphere
import Dodgy.Objects.Cube
import Dodgy.Objects.Cylinder
import Dodgy.Objects.Cone as Cone

drawShuttle :: State -> ObjectAttributes -> IO ()
drawShuttle state object@(ObjectAttributes rotation scaleSize paint location noseVector upVector _ _ _ _ _ _) = do
  

  case (location, scaleSize) of
    ((Just (lx, ly, lz)), (Just s))-> do 

    let wid  = 0.05
        nose = 0.50
        cone = 0.20
        strk = (-0.20)
        tail = (-0.50)
        tex = textures state
        steel' = steel tex
        comb' = comb tex
        

    let paintColor    = color4f snowGray
        shine         = 100
        -- ambs  = (Point4 0 0 0 1.0)
        -- diffs = (Point4 0.01 0.01 0.01 1.0)
        -- specs = (Point4 0.5 0.5 0.5 1.0)
        ambs  = (Point4 0.25 0.25 0.25 1.0)
        diffs = (Point4 0.4 0.4 0.4 1.0)
        specs = (Point4 0.775 0.775 0.775 1.0)
        attrsOverride = ObjectAttributes {
          rotation   = Nothing,
          scaleSize  = Nothing,
          paint      = Nothing,
          location   = Nothing,
          noseVector = Nothing,
          upVector   = Nothing,
          ambience4  = Just ambs,
          diffuse4   = Just diffs, 
          specular4  = Just specs,
          emission4  = Nothing,
          shininess  = Just shine,
          collider   = Nothing
          }
    
    ambient4f ambs
    specular4f specs
    diffuse4f diffs
    drawLightingEffects attrsOverride

    
    -- Capsule Cylindar
    preservingMatrix $ do
      preservingAttrib [AllServerAttributes] $ do
        
        --color3f cx cy cz
        paintColor
        translate $ vector3f lx ly lz
        --multMatrix (mat :: GLmatrix GLfloat)
        let ns = s/14
        scale3f s ns ns

        texture Texture2D $= Enabled
        textureBinding Texture2D $= Just steel'
        textureFilter Texture2D $= ((Nearest, Nothing), Nearest)

        cylinder cone tail
        
    -- Capsule Tail Cap
    preservingMatrix $ do
      preservingAttrib [AllServerAttributes] $ do
        
        -- Offset, scale and rotate
        --color3f cx cy cz
        paintColor
        translate $ vector3f ((lx+tail)*s) ly lz
        --multMatrix (mat :: GLmatrix GLfloat)
        let ns = s/14
        scale3f (ns/3) ns ns

        texture Texture2D $= Enabled
        textureBinding Texture2D $= Just steel'
        textureFilter Texture2D $= ((Nearest, Nothing), Nearest)

        sphere

    -- Cockpit Back
    preservingMatrix $ do
      preservingAttrib [AllServerAttributes] $ do        
        translate $ vector3f ((lx+cone)*s) ly lz
        --multMatrix (mat :: GLmatrix GLfloat)
        let ns = s/14
        scale3f (ns/3) ns ns 
        

        color4f snowGray
        sphere

    -- Cockpit Floor
    preservingMatrix $ do
      preservingAttrib [AllServerAttributes] $ do

        --color3f cx cy cz          
        translate $ vector3f ((lx+cone)*s) ((ly+0.03)*s) lz
        --multMatrix (mat :: GLmatrix GLfloat)
        let ns = s/14
        scale3f (ns*2.7) (ns/6) ns

        color4f snowGray
        sphere
  
    -- Cockpit
    preservingMatrix $ do
      preservingAttrib [AllServerAttributes] $ do
        
        --color3f cx cy cz

        translate $ vector3f ((lx+cone)*s) ((ly+0.025)*s) lz
        --multMatrix (mat :: GLmatrix GLfloat)
        let ns = s/16
            zs = ns*3
            ws = ns*1.1
        scale3f zs ws ws

        blend $= Enabled
        blendFunc $= (SrcAlpha, OneMinusSrcAlpha)

        color4f (Point4 0 0 0 0.4)
        sphere
    
    -- Nose Sphere
    preservingMatrix $ do
      preservingAttrib [AllServerAttributes] $ do
        
        --color3f cx cy cz
        paintColor
        --multMatrix (mat :: GLmatrix GLfloat)
        translate $ vector3f ((lx+cone)*s) ly lz
        
        
        let ns = s/14
            zs = ns*3
            ws = ns*1.1
        scale3f zs ws ws
        

        texture Texture2D $= Enabled
        textureBinding Texture2D $= Just steel'
        textureFilter Texture2D $= ((Nearest, Nothing), Nearest)

        sphere
          
   
    -- Wings
    preservingMatrix $ do
      preservingAttrib [AllServerAttributes] $ do
        
        --color3f cx cy cz
        paintColor
        translate $ vector3f lx ly lz
        --multMatrix (mat :: GLmatrix GLfloat)
        scale3f s s s
        
        texture Texture2D $= Enabled
        textureBinding Texture2D $= Just steel'
        textureFilter Texture2D $= ((Nearest, Nothing), Nearest)

        let wy = 0.075
            woz = 0.25
            wb = (-0.05)

        -- Right Top Base Wing
        renderPrimitive Quads $ do
          drawNormal3f 0 1 (-1)
          drawTexCoord2f 0 1
          drawVertex3f 0 0 0
          drawTexCoord2f 0 0
          drawVertex3f tail 0 0
          drawTexCoord2f 1 0
          drawVertex3f tail wy woz
          drawTexCoord2f 1 1
          drawVertex3f (tail/2) wy woz

        -- Right Top Tip Wing
        renderPrimitive Triangles $ do
          drawNormal3f 0 1 1
          drawTexCoord2f 0 0
          drawVertex3f tail wy woz
          drawTexCoord2f 0 1
          drawVertex3f (tail/2) wy woz
          drawTexCoord2f 1 0.5
          drawVertex3f tail 0 (woz*2)

        -- Right Bottom Base Wing
        renderPrimitive Quads $ do
          drawNormal3f 0 (-1) 1
          drawTexCoord2f 0 1
          drawVertex3f 0 wb 0
          drawTexCoord2f 0 0
          drawVertex3f tail wb 0
          drawTexCoord2f 1 0
          drawVertex3f tail (wy+(wb/2)) woz
          drawTexCoord2f 1 1
          drawVertex3f (tail/2) (wy+(wb/2)) woz

        -- Right Bottom Tip Wing
        renderPrimitive Triangles $ do
          drawNormal3f 0 (-1) (-1)
          drawTexCoord2f 0 0
          drawVertex3f tail (wy+(wb/2)) woz
          drawTexCoord2f 0 1
          drawVertex3f (tail/2) (wy+(wb/2)) woz
          drawTexCoord2f 1 0.5
          drawVertex3f tail 0 (woz*2)

        -- Right Back Base Wing Cover
        renderPrimitive Quads $ do
          drawNormal3f (-1) 0 0
          drawTexCoord2f 0 1
          drawVertex3f tail 0 0
          drawTexCoord2f 1 0
          drawVertex3f tail wy woz
          drawTexCoord2f 1 1
          drawVertex3f tail (wy+(wb/2)) woz
          drawTexCoord2f 0 0
          drawVertex3f tail wb 0

        -- Right Back Tip Wing Cover
        renderPrimitive Triangles $ do
          drawNormal3f (-1) 0 0
          drawTexCoord2f 0 0
          drawVertex3f tail wy woz
          drawTexCoord2f 0 0.5
          drawVertex3f tail (wy+(wb/2)) woz
          drawTexCoord2f 1 0.5
          drawVertex3f tail 0 (woz*2)

        -- Right Front Base Wing Cover
        renderPrimitive Quads $ do
          drawNormal3f 1 0 0
          drawTexCoord2f 0 1
          drawVertex3f 0 0 0
          drawTexCoord2f 1 0
          drawVertex3f (tail/2) wy woz
          drawTexCoord2f 1 1
          drawVertex3f (tail/2) (wy+(wb/2)) woz
          drawTexCoord2f 0 0
          drawVertex3f 0 wb 0

        -- Right Front Base Tip Wing Cover
        renderPrimitive Triangles $ do
          drawNormal3f 1 0 1
          drawTexCoord2f 0 0
          drawVertex3f (tail/2) wy woz
          drawTexCoord2f 0 0.5
          drawVertex3f (tail/2) (wy+(wb/2)) woz
          drawTexCoord2f 1 0.5
          drawVertex3f tail 0 (woz*2)

        -- Left Top Base Wing
        renderPrimitive Quads $ do
          drawNormal3f 0 1 1
          drawTexCoord2f 0 1
          drawVertex3f 0 0 0
          drawTexCoord2f 0 0
          drawVertex3f tail 0 0
          drawTexCoord2f 1 0
          drawVertex3f tail wy (-woz)
          drawTexCoord2f 1 1
          drawVertex3f (tail/2) wy (-woz)

        -- Left Top Tip Wing
        renderPrimitive Triangles $ do
          drawNormal3f 0 1 (-1)
          drawTexCoord2f 0 0
          drawVertex3f tail wy (-woz)
          drawTexCoord2f 0 1
          drawVertex3f (tail/2) wy (-woz)
          drawTexCoord2f 1 0.5
          drawVertex3f tail 0 ((-woz)*2)

        -- Left Bottom Base Wing
        renderPrimitive Quads $ do
          drawNormal3f 0 (-1) (-1)
          drawTexCoord2f 0 1
          drawVertex3f 0 wb 0
          drawTexCoord2f 0 0
          drawVertex3f tail wb 0
          drawTexCoord2f 1 0
          drawVertex3f tail (wy+(wb/2)) (-woz)
          drawTexCoord2f 1 1
          drawVertex3f (tail/2) (wy+(wb/2)) (-woz)

        -- Left Bottom Tip Wing
        renderPrimitive Triangles $ do
          drawNormal3f 0 (-1) 1
          drawTexCoord2f 0 0
          drawVertex3f tail (wy+(wb/2)) (-woz)
          drawTexCoord2f 0 1
          drawVertex3f (tail/2) (wy+(wb/2)) (-woz)
          drawTexCoord2f 1 0.5
          drawVertex3f tail 0 ((-woz)*2)

        -- Left Back Base Wing Cover
        renderPrimitive Quads $ do
          drawNormal3f (-1) 0 0
          drawTexCoord2f 0 1
          drawVertex3f tail 0 0
          drawTexCoord2f 1 0
          drawVertex3f tail wy (-woz)
          drawTexCoord2f 1 1
          drawVertex3f tail (wy+(wb/2)) (-woz)
          drawTexCoord2f 0 0
          drawVertex3f tail wb 0

        -- Left Back Base Tip Wing Cover
        renderPrimitive Triangles $ do
          drawNormal3f (-1) 0 0
          drawTexCoord2f 0 0
          drawVertex3f tail wy (-woz)
          drawTexCoord2f 0 0.5
          drawVertex3f tail (wy+(wb/2)) (-woz)
          drawTexCoord2f 1 0.5
          drawVertex3f tail 0 ((-woz)*2)

        -- Left Front Base Wing Cover
        renderPrimitive Quads $ do
          drawNormal3f 1 0 0
          drawTexCoord2f 0 1
          drawVertex3f 0 0 0
          drawTexCoord2f 1 0
          drawVertex3f (tail/2) wy (-woz)
          drawTexCoord2f 1 1
          drawVertex3f (tail/2) (wy+(wb/2)) (-woz)
          drawTexCoord2f 0 0
          drawVertex3f 0 wb 0

        -- Left Front Base Tip Wing Cover
        renderPrimitive Triangles $ do
          drawNormal3f 1 0 1
          drawTexCoord2f 0 0
          drawVertex3f (tail/2) wy (-woz)
          drawTexCoord2f 0 0.5
          drawVertex3f (tail/2) (wy+(wb/2)) (-woz)
          drawTexCoord2f 1 0.5
          drawVertex3f tail 0 ((-woz)*2)

    -- Right Booster Cylinder
    preservingMatrix $ do
      preservingAttrib [AllServerAttributes] $ do
        
        --color3f cx cy cz
        paintColor
        translate $ vector3f lx ly ((lz+0.2)*s)
        --multMatrix (mat :: GLmatrix GLfloat)    
        let ns = s/35
        scale3f s ns ns

        texture Texture2D $= Enabled
        textureBinding Texture2D $= Just steel'
        textureFilter Texture2D $= ((Nearest, Nothing), Nearest)

        cylinder (tail/2) tail

    -- Right Booster Sphere Cap
    preservingMatrix $ do
      preservingAttrib [AllServerAttributes] $ do
        
        --color3f cx cy cz
        paintColor
        translate $ vector3f ((lx+(tail/2))*s) ly ((lz+0.2)*s)
        --multMatrix (mat :: GLmatrix GLfloat)    
        let ns = s/35
            xs = s/14
        scale3f xs ns ns

        texture Texture2D $= Enabled
        textureBinding Texture2D $= Just steel'
        textureFilter Texture2D $= ((Nearest, Nothing), Nearest)

        sphere


    -- Right Booster Cone
    preservingMatrix $ do
      preservingAttrib [AllServerAttributes] $ do
        
        --color3f cx cy cz        
        translate $ vector3f ((lx+tail-0.0125)*s) ly ((lz+0.2)*s)
        --multMatrix (mat :: GLmatrix GLfloat)    
        let ns = s/25
        scale3f ns ns ns
        rotate1f 90 $ vector3f 0 1 0

        texture Texture2D $= Enabled
        textureBinding Texture2D $= Just steel'
        textureFilter Texture2D $= ((Nearest, Nothing), Nearest)

        Cone.cone 1 0

    -- Right Booster Thrust
    preservingMatrix $ do
      preservingAttrib [AllServerAttributes] $ do
        
        --color3f cx cy cz
        translate $ vector3f ((lx+tail)*s) ly ((lz+0.2)*s)
        --multMatrix (mat :: GLmatrix GLfloat)    
        let ns = s/38
            xs = s/10
        scale3f xs ns ns

        texture Texture2D $= Enabled
        textureBinding Texture2D $= Just (star tex)
        textureFilter Texture2D $= ((Nearest, Nothing), Nearest)

        sphere

    -- Left Booster Cylinder
    preservingMatrix $ do
      preservingAttrib [AllServerAttributes] $ do
        
        --color3f cx cy cz
        paintColor
        translate $ vector3f lx ly ((lz-0.2)*s)
        --multMatrix (mat :: GLmatrix GLfloat)    
        let ns = s/35
        scale3f s ns ns

        texture Texture2D $= Enabled
        textureBinding Texture2D $= Just steel'
        textureFilter Texture2D $= ((Nearest, Nothing), Nearest)

        cylinder (tail/2) tail

    -- Left Booster Sphere Cap
    preservingMatrix $ do
      preservingAttrib [AllServerAttributes] $ do
        
        --color3f cx cy cz
        paintColor
        translate $ vector3f ((lx+(tail/2))*s) ly ((lz-0.2)*s)
        --multMatrix (mat :: GLmatrix GLfloat)    
        let ns = s/35
            xs = s/14
        scale3f xs ns ns

        texture Texture2D $= Enabled
        textureBinding Texture2D $= Just steel'
        textureFilter Texture2D $= ((Nearest, Nothing), Nearest)

        sphere

    -- Left Booster Cone
    preservingMatrix $ do
      preservingAttrib [AllServerAttributes] $ do
        
        --color3f cx cy cz
        paintColor
        translate $ vector3f ((lx+tail-0.0125)*s) ly ((lz-0.2)*s)
        --multMatrix (mat :: GLmatrix GLfloat)    
        let ns = s/25
        scale3f ns ns ns
        rotate1f 90 $ vector3f 0 1 0

        texture Texture2D $= Enabled
        textureBinding Texture2D $= Just steel'
        textureFilter Texture2D $= ((Nearest, Nothing), Nearest)

        Cone.cone 1 0

    -- Left Booster Thrust
    preservingMatrix $ do
      preservingAttrib [AllServerAttributes] $ do
        
        --color3f cx cy cz
        translate $ vector3f ((lx+tail)*s) ly ((lz-0.2)*s)
        --multMatrix (mat :: GLmatrix GLfloat)    
        let ns = s/38
            xs = s/10
        scale3f xs ns ns

        texture Texture2D $= Enabled
        textureBinding Texture2D $= Just (star tex)
        textureFilter Texture2D $= ((Nearest, Nothing), Nearest)

        sphere


    -- Center Booster Cone
    preservingMatrix $ do
      preservingAttrib [AllServerAttributes] $ do
        
        --color3f cx cy cz
        paintColor
        translate $ vector3f ((lx+tail-0.07)*s) ly lz
        --multMatrix (mat :: GLmatrix GLfloat)    
        let ns = s/10
        scale3f ns ns ns
        rotate1f 90 $ vector3f 0 1 0

        texture Texture2D $= Enabled
        textureBinding Texture2D $= Just steel'
        textureFilter Texture2D $= ((Nearest, Nothing), Nearest)

        Cone.cone 1 0

    -- Center Booster Thrust
    preservingMatrix $ do
      preservingAttrib [AllServerAttributes] $ do
        
        --color3f cx cy cz        
        translate $ vector3f ((lx+tail-0.01)*s) ly lz
        --multMatrix (mat :: GLmatrix GLfloat)    
        let ns = s/21
            xs = s/5
        scale3f xs ns ns

        texture Texture2D $= Enabled
        textureBinding Texture2D $= Just (star tex)
        textureFilter Texture2D $= ((Nearest, Nothing), Nearest)

        sphere
  

    -- Wings
    preservingMatrix $ do
      preservingAttrib [AllServerAttributes] $ do
        
        -- Offset, scale and rotate
        --color3f cx cy cz
        paintColor
        translate $ vector3f lx ly lz
        --multMatrix (mat :: GLmatrix GLfloat)
        scale3f s s s

        texture Texture2D $= Enabled
        textureBinding Texture2D $= Just steel'
        textureFilter Texture2D $= ((Nearest, Nothing), Nearest)

        let wz = 0.02
            wbz = 2*wz
        -- Top Fin Wing
        renderPrimitive Triangles $ do

          -- Right Fin
          drawNormal3f 0 1 1 
          drawTexCoord2f 1 0
          drawVertex3f strk 0.0 wz
          drawTexCoord2f 0 1
          drawVertex3f tail 0.3 0
          drawTexCoord2f 0 0
          drawVertex3f tail 0.0 wbz

          -- Left Fin
          drawNormal3f 0 1 (-1)
          drawTexCoord2f 1 0
          drawVertex3f strk 0.0 (-wz)
          drawTexCoord2f 0 1
          drawVertex3f tail 0.3 0
          drawTexCoord2f 0 0
          drawVertex3f tail 0.0 (-wbz)

          -- Front Fin
          drawNormal3f 1 1 0
          drawTexCoord2f 0 1
          drawVertex3f tail 0.3 0
          drawTexCoord2f 1 0
          drawVertex3f strk 0.0 (-wz)
          drawTexCoord2f 0 0
          drawVertex3f strk 0.0 wz
          
          -- Back fin
          drawNormal3f (-1) 0 0
          drawTexCoord2f 0 1
          drawVertex3f tail 0.3 0
          drawTexCoord2f 1 0
          drawVertex3f tail 0.0 (-wbz)
          drawTexCoord2f 0 0
          drawVertex3f tail 0.0 wbz
