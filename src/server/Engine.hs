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

--- updates a board given a list of validated moves
updateBoard :: Board -> [[Move]] -> Board
updateBoard b ml = b -- TODO

--- filters out invalid moves
validateMoves :: Board -> Int -> [Move] -> [Move]
validateMoves b playerId ml = ml -- TODO

--- filters out entities not visible to a player
filterVisible :: Board -> Int -> Board
filterVisible b playerId = b -- TODO