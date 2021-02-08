{-
Copyright 2020 Justin Hu

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

module Util where

import System.Random

type Vec2 = (Double, Double)

type Vec3i = (Int, Int, Int)

magnitude :: Vec2 -> Double
magnitude (x, y) = sqrt (x ** 2 + y ** 2)

distance :: Vec2 -> Vec2 -> Double
distance (ax, ay) (bx, by) = magnitude (ax - bx, ay - by)

vecAdd :: Vec2 -> Vec2 -> Vec2
vecAdd (ax, ay) (bx, by) = (ax + bx, ay + by)

vec3iAdd :: Vec3i -> Vec3i -> Vec3i
vec3iAdd (ax, ay, az) (bx, by, bz) = (ax + bx, ay + by, az + bz)

roll1d6 :: IO Int
roll1d6 = getStdRandom (randomR (1, 6))