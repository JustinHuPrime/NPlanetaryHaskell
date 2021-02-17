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

module TestUtils where

import Board
import Data.Char
import Move
import Serializing
import Test.QuickCheck

limitPrecision :: RealFrac a => a -> Double
limitPrecision x = fromIntegral (round x :: Int) / fixedPointPrecision

forceAscii :: String -> String
forceAscii = filter (\c -> 32 <= ord c && ord c <= 126)

instance Arbitrary Entity where
  arbitrary =
    oneof
      [ do
          idNum <- arbitrary
          x <- (arbitrary :: Gen Double)
          y <- (arbitrary :: Gen Double)
          name <- arbitrary
          mass <- (arbitrary :: Gen Double)
          radius <- (arbitrary :: Gen Double)
          return (AstroObj idNum (limitPrecision x, limitPrecision y) (forceAscii name) (limitPrecision mass) (limitPrecision radius)),
        do
          idNum <- arbitrary
          x <- (arbitrary :: Gen Double)
          y <- (arbitrary :: Gen Double)
          return (AsteroidCluster idNum (limitPrecision x, limitPrecision y)),
        do
          idNum <- arbitrary
          x <- (arbitrary :: Gen Double)
          y <- (arbitrary :: Gen Double)
          dx <- (arbitrary :: Gen Double)
          dy <- (arbitrary :: Gen Double)
          owner <- arbitrary
          name <- arbitrary
          strength <- arbitrary
          isDefensive <- arbitrary
          fuelCap <- (arbitrary :: Gen Double)
          fuel <- (arbitrary :: Gen Double)
          weaponDamage <- arbitrary
          driveDamage <- arbitrary
          structureDamage <- arbitrary
          return (Ship idNum (limitPrecision x, limitPrecision y) (limitPrecision dx, limitPrecision dy) owner (forceAscii name) strength isDefensive (limitPrecision fuelCap) (limitPrecision fuel) weaponDamage driveDamage structureDamage)
      ]

instance Arbitrary Move where
  arbitrary =
    oneof
      [ do
          idNum <- arbitrary
          x <- (arbitrary :: Gen Double)
          y <- (arbitrary :: Gen Double)
          return (Thrust idNum (limitPrecision x, limitPrecision y)),
        do
          attacker <- arbitrary
          target <- arbitrary
          return (Attack attacker target)
      ]