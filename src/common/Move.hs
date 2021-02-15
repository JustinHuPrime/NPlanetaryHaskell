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

module Move where

import qualified Data.ByteString.Char8 as B
import Data.Maybe
import Serializing
import Util

data Move
  = --- thust the ship with given id the given amount
    Thrust Int Vec2
  | --- have the first ship attack the second entity (currently must be a ship)
    Attack Int Int
  deriving (Show, Eq)

--- serializes a list of moves into a list of serialized moves separated by file separators and terminated by an end-of-transmission byte
serializeMoveList :: [Move] -> B.ByteString
serializeMoveList ml = B.intercalate (B.singleton '\x1C') (map serializeMove ml) `B.snoc` '\x03'

--- serializes a move, avoiding the file separator and end-of-transmission bytes
--- serialized move consists of a series of things separated with group separators
serializeMove :: Move -> B.ByteString
serializeMove (Thrust idNum (dx, dy)) =
  serializeGroupList
    [ B.pack "Thrust",
      serializeInt idNum,
      serializeDouble dx,
      serializeDouble dy
    ]
serializeMove (Attack attacker target) =
  serializeGroupList
    [ B.pack "Attack",
      serializeInt attacker,
      serializeInt target
    ]

--- parses a bytestring representing a list of moves into a list of moves, ignoring invalid ones
--- does not expect the terminating EOT
parseMoveList :: B.ByteString -> [Move]
parseMoveList s = mapMaybe parseMove (B.split '\x1C' s)

--- parses a bytestring representing a move into that move, or nothing
parseMove :: B.ByteString -> Maybe Move
parseMove s = parseMoveHelper (map B.unpack (B.split '\x1D' s))
  where
    parseMoveHelper ["Thrust", idNum, dx, dy] = do
      idNum' <- parseInt idNum
      dx' <- parseDouble dx
      dy' <- parseDouble dy
      return (Thrust idNum' (dx', dy'))
    parseMoveHelper ["Attack", attacker, target] = do
      attacker' <- parseInt attacker
      target' <- parseInt target
      return (Attack attacker' target')
    parseMoveHelper _ = Nothing