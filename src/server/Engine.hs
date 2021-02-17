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

module Engine where

import Board
import Move
import Util

--- updates a board given a list of validated moves
updateBoard :: Board -> [[Move]] -> Board
updateBoard b ml = b -- TODO

--- filters out invalid moves
validateMoves :: Board -> Int -> [Move] -> [Move]
validateMoves b playerId = filter (validMove b playerId)

validMove :: Board -> Int -> Move -> Bool
validMove b playerId (Thrust idNum vec) =
  case ship of
    Just (Ship _ _ _ owner _ _ _ _ fuel _ driveDamage _) ->
      owner == playerId
        && driveDamage == 0
        && magnitude vec <= 1
        && magnitude vec <= fuel
    _ -> False
  where
    ship = findId idNum b
validMove b playerId (Attack attacker target) =
  case attackerShip of
    Just (Ship _ _ _ owner _ _ _ _ _ weaponDamage _ _) ->
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
filterVisible b playerId = b -- TODO