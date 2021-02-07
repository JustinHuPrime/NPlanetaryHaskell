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
  | --- Mine with some id, some x, y position and some dx, dy velocity, and some lifetime
    Mine Int Vec2 Vec2 Int
  | --- Torpedo with some id, some x, y position and some dx, dy velocity, and some lifetime
    Torpedo Int Vec2 Vec2 Int
  | --- Nuke with some id, some x, y position and some dx, dy velocity, and some lifetime
    Nuke Int Vec2 Vec2 Int
  | --- Ship with:
    ---  - some id
    ---  - some x, y position
    ---  - some dx, dy velocity
    ---  - an owner
    ---  - a name
    ---  - combat strength
    ---  - whether or not it's defensive only
    ---  - fuel capacity
    ---  - fuel quantity
    ---  - cargo capacity
    ---  - a-list between cargo type and quantity
    Ship Int Vec2 Vec2 Int String Int Bool Int Int Int [(CargoItem, Int)]
  | --- Space Base with some id, some x, y position, an owner, and a name
    SpaceBase Int Vec2 Int String
  | --- Planetside base with some id, some x, y position, an owner, and a name
    PlanetBase Int Vec2 Int String
  deriving (Show)

data CargoItem
  = --- a mine
    MineItem
  | --- a torpedo
    TorpedoItem
  | --- a nuke
    NukeItem
  deriving (Show)