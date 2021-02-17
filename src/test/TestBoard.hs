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

module TestBoard where

import Board
import qualified Data.ByteString.Char8 as B
import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import TestUtils

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

group :: Test
group =
  testGroup
    "board serializer tests"
    [ testProperty "ends with EOT" prop_endsWithEOT,
      testProperty "round trip" prop_roundTrip
    ]

prop_endsWithEOT :: Board -> Bool
prop_endsWithEOT b = B.last (serializeBoard b) == '\x03'

prop_roundTrip :: Board -> Bool
prop_roundTrip b = b == parseBoard (B.init (serializeBoard b))