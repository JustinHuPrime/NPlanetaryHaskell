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

module TestOrderValidator where

import Board
import Engine
import Move
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

group :: Test
group =
  testGroup
    "order validator tests"
    [ testGroup
        "thrust order tests"
        [ testCase "thrust rejects nonexistent thing" test_thrustRejectsNonExistent,
          testCase "thrust rejects non-ship" test_thrustRejectsNonShip,
          testCase "thrust rejects non-owned" test_thrustRejectsNonOwned,
          testCase "thrust rejects damaged ship" test_thrustRejectsDamaged,
          testCase "thrust rejects too large thrust" test_thrustRejectsTooLarge,
          testCase "thrust rejects out of fuel ship" test_thrustRejectsOutOfFuel,
          testCase "thrust accepts in other cases" test_thrustAcceptsOtherCases
        ],
      testGroup
        "attack order tests"
        [ testCase "attack rejects nonexistent thing" test_attackRejectsNonExistent,
          testCase "attack rejects non-ship" test_attackRejectsNonShip,
          testCase "attack rejects non-owned" test_attackRejectsNonOwned,
          testCase "attack rejects damaged ship" test_attackRejectsDamaged,
          testCase "attack rejects nonexistent target" test_attackRejectsNonExistentTarget,
          testCase "attack rejects non-ship target" test_attackRejectsNonShipTarget,
          testCase "attack rejects self-target" test_attackRejectsSelfTarget,
          testCase "attack accepts in other cases" test_attackAcceptsOtherCases
        ]
    ]

test_attackAcceptsOtherCases :: Assertion
test_attackAcceptsOtherCases = validMove [Ship 1 (0, 0) (0, 0) 1 "Ship1" 1 True 10 10 0 0 0, Ship 2 (0, 0) (0, 0) 2 "Ship2" 1 True 10 10 0 0 0] 1 (Attack 1 2) @?= True

test_attackRejectsSelfTarget :: Assertion
test_attackRejectsSelfTarget = validMove [Ship 1 (0, 0) (0, 0) 1 "Ship1" 1 True 10 10 0 0 0, Ship 2 (0, 0) (0, 0) 2 "Ship2" 1 True 10 10 0 0 0] 1 (Attack 1 1) @?= False

test_attackRejectsNonShipTarget :: Assertion
test_attackRejectsNonShipTarget = validMove [Ship 1 (0, 0) (0, 0) 1 "Ship1" 1 True 10 10 0 0 0, AsteroidCluster 2 (0, 0)] 1 (Attack 1 3) @?= False

test_attackRejectsNonExistentTarget :: Assertion
test_attackRejectsNonExistentTarget = validMove [Ship 1 (0, 0) (0, 0) 1 "Ship1" 1 True 10 10 0 0 0, Ship 2 (0, 0) (0, 0) 2 "Ship2" 1 True 10 10 0 0 0] 1 (Attack 1 3) @?= False

test_attackRejectsDamaged :: Assertion
test_attackRejectsDamaged = validMove [Ship 1 (0, 0) (0, 0) 1 "Ship1" 1 True 10 10 1 0 0, Ship 2 (0, 0) (0, 0) 2 "Ship2" 1 True 10 10 0 0 0] 1 (Attack 1 2) @?= False

test_attackRejectsNonOwned :: Assertion
test_attackRejectsNonOwned = validMove [Ship 1 (0, 0) (0, 0) 2 "Ship1" 1 True 10 10 0 0 0, Ship 2 (0, 0) (0, 0) 2 "Ship2" 1 True 10 10 0 0 0] 1 (Attack 1 2) @?= False

test_attackRejectsNonShip :: Assertion
test_attackRejectsNonShip = validMove [AsteroidCluster 1 (0, 0), Ship 2 (0, 0) (0, 0) 1 "Ship2" 1 True 10 5 0 0 0] 1 (Attack 1 2) @?= False

test_attackRejectsNonExistent :: Assertion
test_attackRejectsNonExistent = validMove [Ship 2 (0, 0) (0, 0) 1 "Ship2" 1 True 10 5 0 0 0] 1 (Attack 1 2) @?= False

test_thrustRejectsNonExistent :: Assertion
test_thrustRejectsNonExistent = validMove [] 1 (Thrust 1 (0, 1)) @?= False

test_thrustRejectsNonShip :: Assertion
test_thrustRejectsNonShip = validMove [AsteroidCluster 1 (0, 0)] 1 (Thrust 1 (0, 1)) @?= False

test_thrustRejectsNonOwned :: Assertion
test_thrustRejectsNonOwned = validMove [Ship 1 (0, 0) (0, 0) 2 "Ship1" 1 True 10 10 0 0 0] 1 (Thrust 1 (0, 1)) @?= False

test_thrustRejectsDamaged :: Assertion
test_thrustRejectsDamaged = validMove [Ship 1 (0, 0) (0, 0) 1 "Ship1" 1 True 10 10 0 1 0] 1 (Thrust 1 (0, 1)) @?= False

test_thrustRejectsTooLarge :: Assertion
test_thrustRejectsTooLarge = validMove [Ship 1 (0, 0) (0, 0) 1 "Ship1" 1 True 10 10 0 0 0] 1 (Thrust 1 (0, 2)) @?= False

test_thrustRejectsOutOfFuel :: Assertion
test_thrustRejectsOutOfFuel = validMove [Ship 1 (0, 0) (0, 0) 1 "Ship1" 1 True 10 0.5 0 0 0] 1 (Thrust 1 (0, 1)) @?= False

test_thrustAcceptsOtherCases :: Assertion
test_thrustAcceptsOtherCases = validMove [Ship 1 (0, 0) (0, 0) 1 "Ship1" 1 True 10 5 0 0 0] 1 (Thrust 1 (0, 1)) @?= True