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