module Dodgy.GLUtils where

import Data.IORef ( IORef, newIORef )
import Numeric
import Data.Fixed
import System.Random

import Graphics.Rendering.OpenGL.Raw.ARB.WindowPos
import Graphics.UI.GLUT

type Point3 = (Float, Float, Float)
data Point4 = Point4 Float Float Float Float deriving (Show, Eq)

defd    = 1
loop360 :: [Float]
loop360 = [ p | p <- [0..360], (mod' p defd) == 0]


red      = (Point4 (204/255) 0 0 0)
yellow   = (Point4 1.0 1.0 0.0 1.0)
white    = (Point4 1 1 1 1)
black    = (Point4 0 0 0 1)
blue     = (Point4 (173/255) (216/255) (230/255) 0)
darkGray = (Point4 (50/255) (50/255) (50/255) 0)
snowGray = (Point4 (138/255) (138/255) (138/255) 0)
snowBlue = (Point4 (230/255) (230/255) (250/255) 0)

iToGL :: Int -> GLfloat
iToGL i = (fromIntegral i)::GLfloat

fToGL :: Float -> GLfloat
fToGL f = (realToFrac f)::GLfloat

glCos :: Float -> Float
glCos x = cos(3.1415927/180*x)

glSin :: Float -> Float
glSin x = sin(3.1415927/180*x)

toDeg :: Float -> Float
toDeg x = x*(3.1415927/180)

drawNormal3f :: Float -> Float -> Float -> IO ()
drawNormal3f x y z = currentNormal $= Normal3 ((realToFrac x)::GLfloat) ((realToFrac y)::GLfloat) ((realToFrac z)::GLfloat)

drawVertex3f :: Float -> Float -> Float -> IO ()
drawVertex3f x y z = vertex $ vertex3f x y z

drawTexCoord2f :: Float -> Float -> IO ()
drawTexCoord2f x y = texCoord $ texCoord2f x y

rotate1f :: Float -> Vector3 GLfloat -> IO ()
rotate1f a v = rotate ((realToFrac a)::GLfloat) v

texCoord2f :: Float -> Float -> TexCoord2 GLfloat
texCoord2f x y = TexCoord2 ((realToFrac x)::GLfloat) ((realToFrac y)::GLfloat)

vertex3f :: Float -> Float -> Float -> Vertex3 GLfloat
vertex3f x y z = Vertex3 ((realToFrac x)::GLfloat) ((realToFrac y)::GLfloat) ((realToFrac z)::GLfloat)

vertex3d :: Float -> Float -> Float -> Vertex3 GLdouble
vertex3d x y z = Vertex3 ((realToFrac x)::GLdouble) ((realToFrac y)::GLdouble) ((realToFrac z)::GLdouble)

vertex4f :: Float -> Float -> Float -> Float -> Vertex4 GLfloat
vertex4f x y z w = Vertex4 ((realToFrac x)::GLfloat) ((realToFrac y)::GLfloat) ((realToFrac z)::GLfloat) ((realToFrac w)::GLfloat)

vector3f :: Float -> Float -> Float -> Vector3 GLfloat
vector3f x y z = Vector3 ((realToFrac x)::GLfloat) ((realToFrac y)::GLfloat) ((realToFrac z)::GLfloat)

vector3d :: Float -> Float -> Float -> Vector3 GLdouble
vector3d x y z = Vector3 ((realToFrac x)::GLdouble) ((realToFrac y)::GLdouble) ((realToFrac z)::GLdouble)

scale3f :: Float -> Float -> Float -> IO ()
scale3f x y z = scale ((realToFrac x)::GLfloat) ((realToFrac y)::GLfloat) ((realToFrac z)::GLfloat)

color3f :: Float -> Float -> Float -> IO ()
color3f x y z = color (Color3 ((realToFrac x)::GLfloat) ((realToFrac y)::GLfloat) ((realToFrac z)::GLfloat))

color4f :: Point4 -> IO ()
color4f (Point4 x y z a) = color (Color4  ((realToFrac x)::GLfloat) ((realToFrac y)::GLfloat) ((realToFrac z)::GLfloat) ((realToFrac a)::GLfloat))

setPerspective :: Float -> Float -> Float -> Float -> IO ()
setPerspective fov aspect zNear zFar = perspective ((realToFrac fov)::GLdouble) ((realToFrac aspect)::GLdouble) ((realToFrac zNear)::GLdouble) ((realToFrac zFar)::GLdouble)

setOrtho :: Float -> Float -> Float -> Float -> Float -> Float -> IO ()
setOrtho left right bottom top nearVal farVal = ortho ((realToFrac left)::GLdouble) ((realToFrac right)::GLdouble) ((realToFrac bottom)::GLdouble) ((realToFrac top)::GLdouble) ((realToFrac nearVal)::GLdouble) ((realToFrac farVal)::GLdouble)

setLookAt :: Point3 -> Point3 -> Point3 -> IO ()
setLookAt (ex,ey,ez) (cx,cy,cz) (ux,uy,uz) = lookAt (vertex3d ex ey ez) (vertex3d cx cy cz) (vector3d ux uy uz)

ambient4f :: Point4 -> IO ()
ambient4f (Point4 x y z a) = ambient (Light 0) $= Color4 ((realToFrac x)::GLfloat) ((realToFrac y)::GLfloat) ((realToFrac z)::GLfloat) ((realToFrac a)::GLfloat)

specular4f :: Point4 -> IO ()
specular4f (Point4 x y z a) = specular (Light 0) $= Color4 ((realToFrac x)::GLfloat) ((realToFrac y)::GLfloat) ((realToFrac z)::GLfloat) ((realToFrac a)::GLfloat)

diffuse4f :: Point4 -> IO ()
diffuse4f (Point4 x y z a) = diffuse (Light 0) $= Color4 ((realToFrac x)::GLfloat) ((realToFrac y)::GLfloat) ((realToFrac z)::GLfloat) ((realToFrac a)::GLfloat)

position4f :: Point4 -> IO ()
position4f (Point4 x y z a) = position (Light 0) $= Vertex4 ((realToFrac x)::GLfloat) ((realToFrac y)::GLfloat) ((realToFrac z)::GLfloat) ((realToFrac a)::GLfloat)

pointToColor4f :: Point4 -> Color4 GLfloat
pointToColor4f (Point4 x y z a) = Color4 ((realToFrac x)::GLfloat) ((realToFrac y)::GLfloat) ((realToFrac z)::GLfloat) ((realToFrac a)::GLfloat)

glWindowPos :: GLfloat -> GLfloat -> IO ()
glWindowPos x y = glWindowPos2f x y

round2 :: Float -> String
round2 x = showFFloat (Just 2) x ""

round2GL :: GLfloat -> String
round2GL x = showGFloat (Just 2) x ""

listf :: [Float] -> [GLfloat]
listf ls = map (\x -> ((realToFrac x)::GLfloat)) ls

