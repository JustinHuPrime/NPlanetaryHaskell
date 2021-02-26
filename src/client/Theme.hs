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


import Board

windowWidth :: Int 
windowWidth = 720

windowHeight :: Int
windowHeight = 720

-- color pallette: https://lospec.com/palette-list/small-sprite-greedy-40
-- TODO: recolor entities and have unique colors for each player
entityColor :: Entity -> (Double, Double, Double)
entityColor AstroObj {} = (1, 0, 0)
entityColor AsteroidCluster {} = (1, 0, 0)
entityColor Ship {} = (1, 0, 0)