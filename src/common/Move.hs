module Move where

import Util

data Move
  = --- thust the ship with given id the given amount
    Thrust Int Vec2
  | --- launch a mine from the given ship
    Attack Int Int
  deriving (Show)