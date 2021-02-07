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

module Board where

import Util

type Board = [Entity]

data Entity
  = --- Astronomical object with some id, some x, y position, some name, some mass, some radius (in natural units)
    AstroObj Int Vec2 String Double Double
  | --- Asteroid cluster with some id, some x, y position
    AsteroidCluster Int Vec2
  | --- Ship with:
    ---  - some id
    ---  - some x, y position
    ---  - some dx, dy velocity
    ---  - an owner
    ---  - a name
    ---  - combat strength
    ---  - whether or not it's defensive only
    ---  - fuel capacity (in delta-v)
    ---  - fuel quantity (in delta-v)
    ---  - Weapon health
    ---  - Drive health
    ---  - Structure health
    Ship Int Vec2 Vec2 Int String Int Bool Double Double Int Int Int
  deriving (Show)