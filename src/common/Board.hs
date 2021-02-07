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