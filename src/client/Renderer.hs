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

import Balance
import Board
import Graphics.Rendering.OpenGL.GL
import Util

worldToCanvas :: Vec2 -> Vec2
worldToCanvas (x, y) = (x / mapBorder, y / mapBorder)

renderCircle :: Vec2 -> Double -> Int -> (Double, Double, Double) -> Double -> IO ()
renderCircle (x, y) radius numVertices (r, g, b) layer  = renderPrimitive TriangleFan (sequence_ [color', vertices])
  where
    center    = (x, y)
    arc       = 2.0 * pi / fromIntegral numVertices
    toPoint a = (x + cos a * radius, y + sin a * radius)
    angles    = map ((*arc) . fromIntegral) [0..numVertices]
    points    = center : map toPoint angles
    vertices  = mapM_ (\(x, y) -> vertex (Vertex3 x y layer)) points
    color'    = color $ Color3 r g b

-- TODO: add colors from theme
renderEntity :: Entity -> IO ()
renderEntity (AstroObj _ (x, y) _ _ r) = renderCircle center radius 32 color layer
  where
    center = worldToCanvas (x, y)
    radius = fst (worldToCanvas (r, 0))
    color = (1.0, 0.92, 0.44)
    layer = -0.9
renderEntity (AsteroidCluster _ (x, y)) = renderCircle center radius 32 color layer
  where
    center = worldToCanvas (x, y)
    radius = fst (worldToCanvas (0.5, 0))
    color = (0.8, 0.8, 0.8)
    layer = -0.9
renderEntity (Ship _ (x, y) _ owner _ _ _ _ _ _ _ _) = renderCircle center radius 32 color layer
  where
    center = worldToCanvas (x, y)
    radius = fst (worldToCanvas (0.5, 0))
    color = if owner == 1 then (1.0, 0.25, 0.31) else (0.42, 0.58, 0.94)
    layer = 0.0

renderBoard :: Board -> IO ()
renderBoard = mapM_ renderEntity