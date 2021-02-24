{-
Copyright 2020 Johann Cooper

SPDX-Licence-Identifier: AGPL-3.0-or-later

N-Planetary is free software: you can redistribute it and/or modify it under
the terms of the GNU Affero General Public License as published by the Free
Software Foundation, either version 3 of the License, or (at your option) any
later version.

N-Planetary is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU Affero General Public License along
with N-Planetary. If not, see <https://www.gnu.org/licenses/>.
-}

module Renderer where

import Graphics.Rendering.OpenGL.GL
import Board ( Board, Entity(Ship, AstroObj, AsteroidCluster) )
import Theme
import Util

circle :: (Double, Double) -> Double -> Int -> [(Double, Double)]
circle (x, y) radius vertices = map toPoint angles
  where
    arc       = 2.0 * pi / fromIntegral vertices
    toPoint a = (x + cos a * radius, y + sin a * radius)
    angles    = map ((*arc) . fromIntegral) [0..vertices]

renderFan :: [(Double, Double)] -> Double -> IO ()
renderFan points layer = renderPrimitive TriangleFan $ mapM_ (\(x, y) -> vertex (Vertex3 x y layer)) points

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