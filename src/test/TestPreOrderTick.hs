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

module TestPreOrderTick where

import Board
import Engine
import Balance
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

group :: Test
group =
  testGroup
    "pre order tick tests"
    [ 
      testCase "Reduces all damage by one unless it is max damage" test_reducesDamage
    ]

test_reducesDamage :: Assertion
test_reducesDamage = preOrderTick [
  Ship {Board.idNum = 2, 
        position = (0, 5), 
        velocity = (0, 0), 
        owner = 1, 
        name = "P1 Cruiser", 
        strength = 8, 
        isDefensive = False, 
        fuelCapacity = 20, 
        fuel = 20, 
        weaponDamage = 0, 
        driveDamage = 0, 
        structureDamage = 0},
  Ship {Board.idNum = 4, 
        position = (5, 5), 
        velocity = (0, 0), 
        owner = 2, 
        name = "P2 Ship", 
        strength = 6, 
        isDefensive = False, 
        fuelCapacity = 20, 
        fuel = 13, 
        weaponDamage = 3, 
        driveDamage = 2, 
        structureDamage = maxDamage},
  Ship {Board.idNum = 7, 
        position = (3, 4), 
        velocity = (2, 0.3), 
        owner = 1, 
        name = "P1 Cruiser", 
        strength = 45, 
        isDefensive = False, 
        fuelCapacity = 20, 
        fuel = 20, 
        weaponDamage = maxDamage, 
        driveDamage = maxDamage, 
        structureDamage = 2}] @?= [
  Ship {Board.idNum = 2, 
        position = (0, 5), 
        velocity = (0, 0), 
        owner = 1, 
        name = "P1 Cruiser", 
        strength = 8, 
        isDefensive = False, 
        fuelCapacity = 20, 
        fuel = 20, 
        weaponDamage = 0, 
        driveDamage = 0, 
        structureDamage = 0},
  Ship {Board.idNum = 4, 
        position = (5, 5), 
        velocity = (0, 0), 
        owner = 2, 
        name = "P2 Ship", 
        strength = 6, 
        isDefensive = False, 
        fuelCapacity = 20, 
        fuel = 13, 
        weaponDamage = 2, 
        driveDamage = 1, 
        structureDamage = maxDamage},
  Ship {Board.idNum = 7, 
        position = (3, 4), 
        velocity = (2, 0.3), 
        owner = 1, 
        name = "P1 Cruiser", 
        strength = 45, 
        isDefensive = False, 
        fuelCapacity = 20, 
        fuel = 20, 
        weaponDamage = maxDamage, 
        driveDamage = maxDamage, 
        structureDamage = 1}]
