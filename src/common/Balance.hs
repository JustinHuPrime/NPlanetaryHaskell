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

--- max distance to an ordnance a ship can be before it's hit (natural units)
ordnanceRadius :: Double
ordnanceRadius = 1

--- blast radius of a nuke
nukeRadius :: Double
nukeRadius = 1

--- max distance to a ship/ordnance that a player's ship can see
shipViewRadius :: Double
shipViewRadius = 3

--- max distance to a ship/ordnance that a player's base can see
baseViewRadius :: Double
baseViewRadius = 5

--- max distance away from the sun anything can go before it's 'destroyed'
mapBorder :: Double
mapBorder = 50