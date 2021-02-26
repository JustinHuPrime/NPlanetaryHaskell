{-
Copyright 2020 Justin Hu, Bronwyn Damm

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

module TestMoveResolver where

import Board
import Engine
import Move
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
import System.Random

group :: Test
group =
  testGroup
    "move resolver tests"
    [ testGroup
        "thrust order tests"
        [ testCase "thrusts a ship a small amount" test_simpleThrust
        ],
      testGroup
        "attack order tests"
        [ testCase "simple attack" test_simpleAttack
        ]
    ]

simpleTestBoard :: Board
simpleTestBoard = [ newShip Cruiser 2 (0, 5) (0.1, 0.3) 1 "P1 Cruiser",
    newShip Cruiser 3 (0, -5) (0, 0) 2 "P2 Cruiser"
  ]

test_simpleThrust :: Assertion
test_simpleThrust = do actual <- resolveOrder simpleTestBoard Thrust {Move.idNum = 2, dv = (0.4, 0.3)}
                       actual @?= [ Ship {Board.idNum = 2, position = (0, 5), velocity = (0.5, 0.6), owner = 1, name = "P1 Cruiser", strength = 8, isDefensive = False, fuelCapacity = 20, fuel = 19.5, weaponDamage = 0, driveDamage = 0, structureDamage = 0},
                          Ship {Board.idNum = 3, position = (0, -5), velocity = (0, 0), owner = 2, name = "P2 Cruiser", strength = 8, isDefensive = False, fuelCapacity = 20, fuel = 20, weaponDamage = 0, driveDamage = 0, structureDamage = 0}
                          ]

test_simpleAttack :: Assertion
test_simpleAttack = do setStdGen (mkStdGen 5)
                       actual <- resolveOrder simpleTestBoard Attack {attacker = 2, target = 3}
                       actual @?= [ Ship {Board.idNum = 2, position = (0, 5), velocity = (0.1, 0.3), owner = 1, name = "P1 Cruiser", strength = 8, isDefensive = False, fuelCapacity = 20, fuel = 20, weaponDamage = 0, driveDamage = 0, structureDamage = 0},
                          Ship {Board.idNum = 3, position = (0, -5), velocity = (0, 0), owner = 2, name = "P2 Cruiser", strength = 8, isDefensive = False, fuelCapacity = 20, fuel = 20, weaponDamage = 0, driveDamage = 2, structureDamage = 0}
                          ]