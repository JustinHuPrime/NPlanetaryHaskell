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

module Balance where

import Util

--- max distance to a ship/ordnance that a player's ship can see
shipViewRadius :: Double
shipViewRadius = 3

--- max distance away from the sun anything can go before it's 'destroyed'
mapBorder :: Double
mapBorder = 50

--- max delta-v a ship can provide in a normal thrust
thrustDeltaV :: Double
thrustDeltaV = 1

--- max damage of any health category
maxDamage :: Int
maxDamage = 6

--- odds ratio of a combat
data OddsRatio
  = Impossible
  | Terrible
  | Bad
  | Even
  | Good
  | Excellent
  | Amazing
  deriving (Eq, Show)

oddsRatio :: Int -> Int -> OddsRatio
oddsRatio attackStrength defendStrength
  | attackStrength * 4 < defendStrength = Impossible
  | attackStrength * 2 < defendStrength = Terrible
  | attackStrength < defendStrength = Bad
  | attackStrength < defendStrength * 2 = Even
  | attackStrength < defendStrength * 3 = Good
  | attackStrength < defendStrength * 4 = Excellent
  | otherwise = Amazing

rollHits :: OddsRatio -> IO Int
rollHits Impossible = return 0
rollHits Terrible = do
  table <$> roll1d6
  where
    table 1 = 0
    table 2 = 0
    table 3 = 0
    table 4 = 0
    table 5 = 0
    table 6 = 1
    table _ = error "invalid 1d6 roll!"
rollHits Bad = do
  table <$> roll1d6
  where
    table 1 = 0
    table 2 = 0
    table 3 = 0
    table 4 = 0
    table 5 = 1
    table 6 = 1
    table _ = error "invalid 1d6 roll!"
rollHits Even = do
  table <$> roll1d6
  where
    table 1 = 0
    table 2 = 0
    table 3 = 0
    table 4 = 1
    table 5 = 1
    table 6 = 2
    table _ = error "invalid 1d6 roll!"
rollHits Good = do
  table <$> roll1d6
  where
    table 1 = 0
    table 2 = 0
    table 3 = 1
    table 4 = 1
    table 5 = 2
    table 6 = 2
    table _ = error "invalid 1d6 roll!"
rollHits Excellent = do
  table <$> roll1d6
  where
    table 1 = 0
    table 2 = 1
    table 3 = 1
    table 4 = 2
    table 5 = 2
    table 6 = 3
    table _ = error "invalid 1d6 roll!"
rollHits Amazing = do
  table <$> roll1d6
  where
    table 1 = 1
    table 2 = 1
    table 3 = 2
    table 4 = 2
    table 5 = 3
    table 6 = 3
    table _ = error "invalid 1d6 roll!"

rollDamage :: Int -> IO (Int, Int, Int)
rollDamage 0 = return (0, 0, 0)
rollDamage hits = do
  dmg1 <- rollOneDamage
  dmg2 <- rollDamage (hits - 1)
  return (dmg1 `vec3iAdd` dmg2)
  where
    rollOneDamage = table <$> roll1d6
      where
        table 1 = (1, 0, 0)
        table 2 = (0, 1, 0)
        table 3 = (0, 0, 1)
        table 4 = (1, 0, 1)
        table 5 = (1, 1, 0)
        table 6 = (0, 1, 1)
        table _ = error "invalid 1d6 roll!"