module Renderer where

import Graphics.Rendering.OpenGL.GL
import Board ( Board, Entity(Ship, AstroObj, AsteroidCluster) )
import Theme
import Util

circle (x, y) radius vertices = map toPoint angles
  where
    arc       = 2.0 * pi / fromIntegral vertices
    toPoint a = (x + cos a * radius, y + sin a * radius)
    angles    = map ((*arc) . fromIntegral) [0..vertices]

renderFan points layer = do
    renderPrimitive TriangleFan $ do
      mapM_ (\(x, y) -> vertex (Vertex3 x y layer)) points

renderCircle :: Vec2 -> Double -> Double -> Int -> IO ()
renderCircle position radius layer vertices = renderFan (position : circle position radius vertices) layer

renderBoard :: Board -> IO ()
renderBoard = mapM_ renderEntity

-- TODO: add colors from theme
renderEntity :: Entity -> IO ()
renderEntity (AstroObj _ (x, y) _ _ radius) = renderCircle position radius' layer 32
  where
    position = (x / fromIntegral windowWidth, y / fromIntegral windowHeight)
    radius' = radius / fromIntegral windowWidth
    layer = -0.9
renderEntity (AsteroidCluster _ (x, y))          = renderCircle position radius layer 32
  where
    position = (x / fromIntegral windowWidth, y / fromIntegral windowHeight)
    radius = 10.0 / fromIntegral windowWidth
    layer = -0.9
renderEntity (Ship _ (x, y) _ _ _ _ _ _ _ _ _ _) = renderCircle position radius layer 32
  where
    position = (x / fromIntegral windowWidth, y / fromIntegral windowHeight)
    radius = 10.0 / fromIntegral windowWidth
    layer = 0.0