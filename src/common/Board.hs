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

module Board where

import qualified Data.ByteString.Char8 as B
import Data.List
import Data.Maybe
import Serializing
import Util

type Board = [Entity]

data Entity
  = --- Astronomical object with some id, some x, y position, some name, some mass, some radius (in natural units)
    AstroObj
      { idNum :: Int,
        position :: Vec2,
        name :: String,
        mass :: Double,
        radius :: Double
      }
  | --- Asteroid cluster with some id, some x, y position
    AsteroidCluster
      { idNum :: Int,
        position :: Vec2
      }
  | --- Ship with:
    ---  - some id
    ---  - some x, y position
    ---  - some dx, dy velocity
    ---  - an owner
    ---  - a name
    ---  - combat strength
    ---  - whether or not it's defensive only
    ---  - fuel capacity (in delta-v)
    ---  - fuel quantity (in delta-v)
    ---  - Weapon damage
    ---  - Drive damage
    ---  - Structure damage
    Ship
      { idNum :: Int,
        position :: Vec2,
        velocity :: Vec2,
        owner :: Int,
        name :: String,
        strength :: Int,
        isDefensive :: Bool,
        fuelCapacity :: Double,
        fuel :: Double,
        weaponDamage :: Int,
        driveDamage :: Int,
        structureDamage :: Int,
        cargoCapacity :: Int,
        isCivilian :: Bool,
        mineQty :: Int,
        torpedoQty :: Int,
        nukeQty :: Int,
        scannerQty :: Int,
        pmGrappleQty :: Int,
        automatedMineQty :: Int,
        robotGuardQty :: Int,
        oreQty :: Int,
        ctShardQty :: Int,
        mcrQty :: Int
      }
  deriving (Show, Eq)

findId :: Int -> Board -> Maybe Entity
findId searchFor = find (\e -> searchFor == idNum e)

isShip :: Entity -> Bool
isShip Ship {} = True
isShip _ = False

doDamage :: (Int, Int, Int) -> Entity -> Entity
doDamage (toWeapon, toDrive, toStructure) ship@Ship {weaponDamage, driveDamage, structureDamage} =
  ship
    { weaponDamage = min (weaponDamage + toWeapon) 6,
      driveDamage = min (driveDamage + toDrive) 6,
      structureDamage = min (structureDamage + toStructure) 6
    }
doDamage _ _ = error "Damage applied to a non-ship!"

--- computes the acceleration vector due to gravity at a given position
--- uses inverse-linear gravity (a = M/r)
gravityAt :: Board -> Vec2 -> Vec2
gravityAt b shipPos = foldr (vecAdd . gravityFrom) (0, 0) b
  where
    gravityFrom AstroObj {position, mass} = magnitude (position `vecMinus` shipPos) `vecDiv` (mass `vecTimes` vecUnit (position `vecMinus` shipPos))
    gravityFrom _ = (0, 0)

--- is this entity always visible?
alwaysVisible :: Entity -> Bool
alwaysVisible AstroObj {} = True
alwaysVisible AsteroidCluster {} = True
alwaysVisible _ = False

--- is ship owned by given player
isPlayerShip :: Int -> Entity -> Bool
isPlayerShip playerId Ship {owner} = playerId == owner
isPlayerShip _ _ = False

--- serializes a board into a list of serialized entities separated by file separators and terminated by an end-of-transmission (EOT) byte
serializeBoard :: Board -> B.ByteString
serializeBoard b = B.intercalate (B.singleton '\x1C') (map serializeEntity b) `B.snoc` '\x03'

--- serializes an entity, avoiding the file separator and end-of-transmission bytes
--- serialized entity consists of a series of things separated with group separators
serializeEntity :: Entity -> B.ByteString
serializeEntity (AstroObj idNum (x, y) name mass radius) =
  serializeGroupList
    [ B.pack "AstroObj",
      serializeInt idNum,
      serializeDouble x,
      serializeDouble y,
      B.pack name,
      serializeDouble mass,
      serializeDouble radius
    ]
serializeEntity (AsteroidCluster idNum (x, y)) =
  serializeGroupList
    [ B.pack "AsteroidCluster",
      serializeInt idNum,
      serializeDouble x,
      serializeDouble y
    ]
serializeEntity
  ( Ship
      idNum
      (x, y)
      (dx, dy)
      owner
      name
      strength
      isDefensive
      fuelCap
      fuel
      weaponDamage
      driveDamage
      structureDamage
      cargoCapacity
      isCivilian
      mineQty
      torpedoQty
      nukeQty
      scannerQty
      pmGrappleQty
      automatedMineQty
      robotGuardQty
      oreQty
      ctShardQty
      mcrQty
    ) =
    serializeGroupList
      [ B.pack "Ship",
        serializeInt idNum,
        serializeDouble x,
        serializeDouble y,
        serializeDouble dx,
        serializeDouble dy,
        serializeInt owner,
        B.pack name,
        serializeInt strength,
        serializeBool isDefensive,
        serializeDouble fuelCap,
        serializeDouble fuel,
        serializeInt weaponDamage,
        serializeInt driveDamage,
        serializeInt structureDamage,
        serializeInt cargoCapacity,
        serializeBool isCivilian,
        serializeInt mineQty,
        serializeInt torpedoQty,
        serializeInt nukeQty,
        serializeInt scannerQty,
        serializeInt pmGrappleQty,
        serializeInt automatedMineQty,
        serializeInt robotGuardQty,
        serializeInt oreQty,
        serializeInt ctShardQty,
        serializeInt mcrQty
      ]

--- parses a bytestring representing a board into a list of entities, ignoring invalid ones
--- does not expect the terminating EOT
parseBoard :: B.ByteString -> Board
parseBoard s = mapMaybe parseEntity (B.split '\x1C' s)

--- parses a bytestring representing an entity into that entity, or nothing if it's invalid
parseEntity :: B.ByteString -> Maybe Entity
parseEntity s = parseEntityHelper (map B.unpack (B.split '\x1D' s))
  where
    parseEntityHelper ["AstroObj", idNum, x, y, name, mass, radius] = do
      idNum' <- parseInt idNum
      x' <- parseDouble x
      y' <- parseDouble y
      mass' <- parseDouble mass
      radius' <- parseDouble radius
      return (AstroObj idNum' (x', y') name mass' radius')
    parseEntityHelper ["AsteroidCluster", idNum, x, y] = do
      idNum' <- parseInt idNum
      x' <- parseDouble x
      y' <- parseDouble y
      return (AsteroidCluster idNum' (x', y'))
    parseEntityHelper
      [ "Ship",
        idNum,
        x,
        y,
        dx,
        dy,
        owner,
        name,
        strength,
        isDefensive,
        fuelCap,
        fuel,
        weaponDamage,
        driveDamage,
        structureDamage,
        cargoCapacity,
        isCivilian,
        mineQty,
        torpedoQty,
        nukeQty,
        scannerQty,
        pmGrappleQty,
        automatedMineQty,
        robotGuardQty,
        oreQty,
        ctShardQty,
        mcrQty
        ] = do
        idNum' <- parseInt idNum
        x' <- parseDouble x
        y' <- parseDouble y
        dx' <- parseDouble dx
        dy' <- parseDouble dy
        owner' <- parseInt owner
        strength' <- parseInt strength
        isDefensive' <- parseBool isDefensive
        fuelCap' <- parseDouble fuelCap
        fuel' <- parseDouble fuel
        weaponDamage' <- parseInt weaponDamage
        driveDamage' <- parseInt driveDamage
        structureDamage' <- parseInt structureDamage
        cargoCapacity' <- parseInt cargoCapacity
        isCivilian' <- parseBool isCivilian
        mineQty' <- parseInt mineQty
        torpedoQty' <- parseInt torpedoQty
        nukeQty' <- parseInt nukeQty
        scannerQty' <- parseInt scannerQty
        pmGrappleQty' <- parseInt pmGrappleQty
        automatedMineQty' <- parseInt automatedMineQty
        robotGuardQty' <- parseInt robotGuardQty
        oreQty' <- parseInt oreQty
        ctShardQty' <- parseInt ctShardQty
        mcrQty' <- parseInt mcrQty
        return
          ( Ship
              idNum'
              (x', y')
              (dx', dy')
              owner'
              name
              strength'
              isDefensive'
              fuelCap'
              fuel'
              weaponDamage'
              driveDamage'
              structureDamage'
              cargoCapacity'
              isCivilian'
              mineQty'
              torpedoQty'
              nukeQty'
              scannerQty'
              pmGrappleQty'
              automatedMineQty'
              robotGuardQty'
              oreQty'
              ctShardQty'
              mcrQty'
          )
    parseEntityHelper _ = Nothing