module Dodgy.Textures where

import Control.Applicative

import Graphics.UI.GLUT
import Graphics.GLUtil

loadGLTextureFromFile :: FilePath -> IO TextureObject
loadGLTextureFromFile f = do 
  t <- either error id <$> readTexture f
  --textureFilter Texture2D $= ((Linear', Nothing), Linear')
  texture2DWrap $= (Mirrored, ClampToEdge)
  return t