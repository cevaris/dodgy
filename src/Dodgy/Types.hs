module Dodgy.Types where

import Data.IORef ( IORef, newIORef )

import Graphics.UI.GLUT

import Dodgy.Objects.Types

data ChangeDirection = Increase | Decrease deriving (Show)

data Direction = UpDirection | DownDirection | LeftDirection | RightDirection deriving (Show, Eq)

data ProjectionView = PerspectiveView | OrthogonalView | FirstPersonView deriving (Show, Eq)


data Difficulty = Hard | Medium | Easy deriving (Show, Eq)


data State = State {
  frames  :: IORef Int,
  t0      :: IORef Int,
  ph'     :: IORef Float,
  th'     :: IORef Float,
  gr'     :: IORef Float,
  zh'     :: IORef Float,
  asp     :: IORef Float,
  fov     :: IORef Float,
  dim     :: IORef Float,
   
  ylight' :: IORef Float,
  rlight' :: IORef Float,
  emiss'  :: IORef Float,
  diff'   :: IORef Float,
  amb'    :: IORef Float,
  spec'   :: IORef Float,
  smooth' :: IORef ShadingModel,
  light'  :: IORef Bool,
  shine'  :: IORef Int,
  move'   :: IORef Bool,

  mpPosX  :: IORef Float,
  mpPosY  :: IORef Float,
  mode    :: IORef Difficulty,
  bricks  :: IORef [Brick],

  textures :: Textures,
   
  info    :: IORef (String,String)
}