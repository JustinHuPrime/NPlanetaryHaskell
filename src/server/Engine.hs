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
{-# LANGUAGE NamedFieldPuns #-}

module Engine where

import Balance
import Board
import Move
import Util

--- updates a board given a list of validated moves
updateBoard :: [Move] -> Board -> Board
updateBoard ml = postOrderTick . resolveOrders ml . preOrderTick

--- updates the board before the orders phase
preOrderTick :: Board -> Board
preOrderTick b = b -- TODO

--- resolves a list of orders
resolveOrders :: [Move] -> Board -> Board
resolveOrders ml b = foldr resolveOrder b ml

--- resolves an order
resolveOrder :: Move -> Board -> Board
resolveOrder Thrust {Move.idNum = idNum', dv = dv'} b = map (\e -> if Board.idNum e == idNum' then e {velocity = velocity e `vecAdd` dv'} else e) b
resolveOrder Attack {attacker, target} b = b -- TODO

--- updates the board after the orders phase
postOrderTick :: Board -> Board
postOrderTick b = b -- TODO

--- filters out invalid moves
validateMoves :: Board -> Int -> [Move] -> [Move]
validateMoves b playerId = filter (validMove b playerId)

validMove :: Board -> Int -> Move -> Bool
validMove b playerId (Thrust idNum vec) =
  case ship of
    Just Ship {owner, fuel, driveDamage} ->
      owner == playerId
        && driveDamage == 0
        && magnitude vec <= 1
        && magnitude vec <= fuel
    _ -> False
  where
    ship = findId idNum b
validMove b playerId Attack {attacker, target} =
  case attackerShip of
    Just Ship {owner, weaponDamage} ->
      owner == playerId
        && weaponDamage == 0
        && attacker /= target
        && validTarget
    _ -> False
  where
    attackerShip = findId attacker b
    validTarget =
      case targetShip of
        Just Ship {} ->
          True
        _ -> False
      where
        targetShip = findId target b

--- filters out entities not visible to a player
filterVisible :: Board -> Int -> Board
filterVisible b playerId =
  filter
    ( \o ->
        alwaysVisible o
          || ( playerShips /= []
                 && minimum (map (\ps -> distance (position ps) (position o)) playerShips) <= shipViewRadius
             )
    )
    b
  where
    playerShips = filter (isPlayerShip playerId) b