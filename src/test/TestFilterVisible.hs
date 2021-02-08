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
{-# OPTIONS_GHC -Wno-orphans #-}

module TestFilterVisible where

import Balance
import Board
import Engine
import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import TestUtils ()
import Util

group :: Test
group =
  testGroup
    "filter visible tests"
    [ testProperty "visible ships are always near player ships" prop_visibleNearPlayerShip
    ]

prop_visibleNearPlayerShip :: Board -> Int -> Bool
prop_visibleNearPlayerShip b playerId =
  all
    ( \o ->
        alwaysVisible o
          || ( playerShips /= []
                 && minimum (map (\ps -> distance (position ps) (position o)) playerShips) <= shipViewRadius
             )
    )
    (filterVisible b playerId)
  where
    playerShips = filter (isPlayerShip playerId) b