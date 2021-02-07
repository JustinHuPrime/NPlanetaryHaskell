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

module Networking where

import Board
import Move
import NetInterface
import Network.Socket
import Network.Socket.ByteString

--- reads a board from the server
readBoard :: Socket -> IO Board
readBoard s = parseBoard <$> readPacket s

--- sends a list of moves down the socket
sendMoves :: Socket -> [Move] -> IO ()
sendMoves s ml = do
  sendAll s (serializeMoveList ml)