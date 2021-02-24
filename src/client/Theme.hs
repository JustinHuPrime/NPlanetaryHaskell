module Theme where

import Graphics.Rendering.OpenGL.GL as GL

import Board

windowWidth :: Int 
windowWidth = 500

windowHeight :: Int
windowHeight = 500

-- TODO: recolor entities and have unique colors for each player
entityColor :: Num a => Entity -> GL.Color3 a
entityColor AstroObj {} = GL.Color3 0 1 0
entityColor AsteroidCluster {} = GL.Color3 1 0 0
entityColor Ship {} = GL.Color3 0 0 1