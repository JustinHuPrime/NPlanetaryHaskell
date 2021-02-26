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

module TestPostOrderTick where

import Board
import Engine
import Balance
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

group :: Test
group =
  testGroup
    "move resolver tests"
    [ 
      testCase "Only destroys ships with enough damage" test_removesAppropDestroyedShips,
      testCase "Ignores non-structural damage when destroying ships" test_ignoresNonStructuralDamage,
      testCase "test movement without any gravity" test_movesShips,
      testCase "test movement with gravity" test_movesShipsWithGravity
    ]

test_removesAppropDestroyedShips :: Assertion
test_removesAppropDestroyedShips = postOrderTick [
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
        weaponDamage = 0, 
        driveDamage = 0, 
        structureDamage = maxDamage - 1},
  Ship {Board.idNum = 7, 
        position = (3, 4), 
        velocity = (2, 0.3), 
        owner = 1, 
        name = "P1 Cruiser", 
        strength = 45, 
        isDefensive = False, 
        fuelCapacity = 20, 
        fuel = 20, 
        weaponDamage = 0, 
        driveDamage = 0, 
        structureDamage = maxDamage}] @?= [
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
        weaponDamage = 0, 
        driveDamage = 0, 
        structureDamage = maxDamage - 1}]

test_ignoresNonStructuralDamage :: Assertion
test_ignoresNonStructuralDamage = postOrderTick [
  Ship {Board.idNum = 4, 
        position = (5, 5), 
        velocity = (0, 0), 
        owner = 2, 
        name = "P2 Ship", 
        strength = 6, 
        isDefensive = False, 
        fuelCapacity = 20, 
        fuel = 13, 
        weaponDamage = 23, 
        driveDamage = 45, 
        structureDamage = maxDamage - 1}] @?= [
  Ship {Board.idNum = 4, 
        position = (5, 5), 
        velocity = (0, 0), 
        owner = 2, 
        name = "P2 Ship", 
        strength = 6, 
        isDefensive = False, 
        fuelCapacity = 20, 
        fuel = 13, 
        weaponDamage = 23, 
        driveDamage = 45, 
        structureDamage = maxDamage - 1}]

test_movesShips :: Assertion
test_movesShips = postOrderTick [
  Ship {Board.idNum = 2, 
        position = (0, 5), 
        velocity = (2, 0), 
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
        velocity = (3, -7), 
        owner = 2, 
        name = "P2 Cruiser", 
        strength = 8, 
        isDefensive = False, 
        fuelCapacity = 20, 
        fuel = 20, 
        weaponDamage = 0, 
        driveDamage = 0, 
        structureDamage = 0}] @?= [
  Ship {Board.idNum = 2, 
        position = (2, 5), 
        velocity = (2, 0), 
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
        position = (8, -2), 
        velocity = (3, -7), 
        owner = 2, 
        name = "P2 Cruiser", 
        strength = 8, 
        isDefensive = False, 
        fuelCapacity = 20, 
        fuel = 20, 
        weaponDamage = 0, 
        driveDamage = 0, 
        structureDamage = 0}]

test_movesShipsWithGravity :: Assertion
test_movesShipsWithGravity = postOrderTick [
  Ship {Board.idNum = 4, 
        position = (8, -2), 
        velocity = (3, -7), 
        owner = 2, 
        name = "P2 Cruiser", 
        strength = 8, 
        isDefensive = False, 
        fuelCapacity = 20, 
        fuel = 20, 
        weaponDamage = 0, 
        driveDamage = 0, 
        structureDamage = 0},
  AstroObj {Board.idNum = 5,
        position = (8, -1),
        name = "Problem Causing",
        mass = 3,
        radius = 0.25
      }] @?= [
  Ship {Board.idNum = 4, 
        position = (11, -9), 
        velocity = (3, -4), 
        owner = 2, 
        name = "P2 Cruiser", 
        strength = 8, 
        isDefensive = False, 
        fuelCapacity = 20, 
        fuel = 20, 
        weaponDamage = 0, 
        driveDamage = 0, 
        structureDamage = 0},
  AstroObj {Board.idNum = 5,
        position = (8, -1),
        name = "Problem Causing",
        mass = 3,
        radius = 0.25
      }]

