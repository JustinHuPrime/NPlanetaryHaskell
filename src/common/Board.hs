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

module Board where

import qualified Data.ByteString.Char8 as B
import Data.Maybe
import Text.Read
import Util

type Board = [Entity]

data Entity
  = --- Astronomical object with some id, some x, y position, some name, some mass, some radius (in natural units)
    AstroObj Int Vec2 String Double Double
  | --- Asteroid cluster with some id, some x, y position
    AsteroidCluster Int Vec2
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
    ---  - Weapon health
    ---  - Drive health
    ---  - Structure health
    Ship Int Vec2 Vec2 Int String Int Bool Double Double Int Int Int
  deriving (Show, Eq)

--- serializes a board into a list of serialized entities separated by file separators and terminated by an end-of-transmission (EOT) byte
serializeBoard :: Board -> B.ByteString
serializeBoard b = B.intercalate (B.singleton '\x1C') (map serializeEntity b) `B.snoc` '\x03'

--- serializes an entity, avoiding the file separator and end-of-transmission bytes
--- serialized entity consists of a series of things separated with group separators
--- subsequent levels use record separators then unit separators
serializeEntity :: Entity -> B.ByteString
serializeEntity (AstroObj idNum (x, y) name mass radius) =
  serializeEntityList
    [ B.pack "AstroObj",
      serializeInt idNum,
      serializeDouble x,
      serializeDouble y,
      B.pack name,
      serializeDouble mass,
      serializeDouble radius
    ]
serializeEntity (AsteroidCluster idNum (x, y)) =
  serializeEntityList
    [ B.pack "AsteroidCluster",
      serializeInt idNum,
      serializeDouble x,
      serializeDouble y
    ]
serializeEntity (Ship idNum (x, y) (dx, dy) owner name strength isDefensive fuelCap fuel weaponHealth driveHealth structureHealth) =
  serializeEntityList
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
      serializeInt weaponHealth,
      serializeInt driveHealth,
      serializeInt structureHealth
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
    parseEntityHelper ["Ship", idNum, x, y, dx, dy, owner, name, strength, isDefensive, fuelCap, fuel, weaponHealth, driveHealth, structureHealth] = do
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
      weaponHealth' <- parseInt weaponHealth
      driveHealth' <- parseInt driveHealth
      structureHealth' <- parseInt structureHealth
      return (Ship idNum' (x', y') (dx', dy') owner' name strength' isDefensive' fuelCap' fuel' weaponHealth' driveHealth' structureHealth')
    parseEntityHelper _ = Nothing

--- serializes an integer as a bytestring
serializeInt :: Int -> B.ByteString
serializeInt = B.pack . show

--- parses an integer as a bytestring
parseInt :: String -> Maybe Int
parseInt s = readMaybe s :: Maybe Int

--- serializes a double as a bytestring
serializeDouble :: Double -> B.ByteString
serializeDouble x = B.pack (show (round (x * fixedPointPrecision) :: Int))

--- parses a double as a bytestring
parseDouble :: String -> Maybe Double
parseDouble s = do
  fixedPoint <- parseInt s
  return (fromIntegral fixedPoint / fixedPointPrecision)

--- precision for fixed point numbers
fixedPointPrecision :: Double
fixedPointPrecision = 10000

--- serializes an entity's list of bytestrings
serializeEntityList :: [B.ByteString] -> B.ByteString
serializeEntityList = B.intercalate (B.singleton '\x1D')

--- serializes a boolean into a bytestring
serializeBool :: Bool -> B.ByteString
serializeBool True = B.singleton 'T'
serializeBool False = B.singleton 'F'

--- parses a boolean from a string
parseBool :: String -> Maybe Bool
parseBool "T" = Just True
parseBool "F" = Just False
parseBool _ = Nothing