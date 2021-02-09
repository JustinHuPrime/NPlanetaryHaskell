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

--- max distance to a ship/ordnance that a player's ship can see
shipViewRadius :: Double
shipViewRadius = 3

--- max distance away from the sun anything can go before it's 'destroyed'
mapBorder :: Double
mapBorder = 50

--- max delta-v a ship can provide in a normal thrust
thrustDeltaV :: Double
thrustDeltaV = 1

--- max health of any health category
maxHealth :: Int
maxHealth = 6