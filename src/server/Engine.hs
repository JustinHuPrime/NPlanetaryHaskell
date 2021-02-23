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
import Control.Monad
import Move
import Util

--- updates a board given a list of validated moves
updateBoard :: [Move] -> Board -> IO Board
updateBoard ml b = postOrderTick <$> resolveOrders ml (preOrderTick b)

--- updates the board before the orders phase
preOrderTick :: Board -> Board
preOrderTick = map preOrderTickOne
  where
    preOrderTickOne :: Entity -> Entity
    preOrderTickOne ship@Ship {weaponDamage, driveDamage, structureDamage} =
      ship
        { weaponDamage = if weaponDamage == maxDamage then maxDamage else max (weaponDamage - 1) 0,
          driveDamage = if driveDamage == maxDamage then maxDamage else max (driveDamage - 1) 0,
          structureDamage = if structureDamage == maxDamage then maxDamage else max (structureDamage - 1) 0
        }
    preOrderTickOne other = other

-- reduce damage of things by one

--- resolves a list of orders
resolveOrders :: [Move] -> Board -> IO Board
resolveOrders ml b = foldM resolveOrder b ml

--- resolves an order
resolveOrder :: Board -> Move -> IO Board
resolveOrder b Thrust {Move.idNum = idNum', dv = dv'} = return (map (\e -> if Board.idNum e == idNum' then e {velocity = velocity e `vecAdd` dv'} else e) b)
resolveOrder b Attack {attacker, target} = mapM resolveAttack b
  where
    attacker' = case findId attacker b of
      Just attacker' -> attacker'
      Nothing -> error "invalid attack order being resolved"
    resolveAttack e =
      if Board.idNum e == target
        then do
          damage <- resolveCombat attacker' e
          return (doDamage damage e)
        else return e

--- resolves a single combat
resolveCombat :: Entity -> Entity -> IO (Int, Int, Int)
resolveCombat attacker target = do
  hits <- rollHits (oddsRatio (strength attacker) (strength target))
  rollDamage hits

--- updates the board after the orders phase
postOrderTick :: Board -> Board
postOrderTick b = filter (not . destroyed) (map motionTick b)
  where
    destroyed :: Entity -> Bool
    destroyed Ship {structureDamage} = structureDamage == maxDamage
    destroyed Mine {timeToLive} = timeToLive == 0
    destroyed Torpedo {timeToLive} = timeToLive == 0
    destroyed Nuke {timeToLive} = timeToLive == 0
    destroyed _ = False

    motionTick :: Entity -> Entity
    motionTick ship@Ship {position, velocity} =
      ship
        { position = position `vecAdd` velocity,
          velocity = velocity `vecAdd` gravityAt b position
        }
    motionTick mine@Mine {position, velocity} =
      mine
        { position = position `vecAdd` velocity,
          velocity = velocity `vecAdd` gravityAt b position
        }
    motionTick torpedo@Torpedo {position, velocity} =
      torpedo
        { position = position `vecAdd` velocity,
          velocity = velocity `vecAdd` gravityAt b position
        }
    motionTick nuke@Nuke {position, velocity} =
      nuke
        { position = position `vecAdd` velocity,
          velocity = velocity `vecAdd` gravityAt b position
        }
    motionTick other = other

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
    Just Ship {owner, weaponDamage, isDefensive} ->
      owner == playerId
        && weaponDamage == 0
        && attacker /= target
        && not isDefensive
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