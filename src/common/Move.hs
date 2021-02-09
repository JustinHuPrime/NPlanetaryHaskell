module Move where

import Util

data Move
  = --- thust the ship with given id the given amount
    Thrust Int Vec2
  | --- have the first ship attack the second entity (currently must be a ship)
    Attack Int Int
  deriving (Show)