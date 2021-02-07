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

module TestBoard where

import Board
import qualified Data.ByteString.Char8 as B
import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import TestUtils ()

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