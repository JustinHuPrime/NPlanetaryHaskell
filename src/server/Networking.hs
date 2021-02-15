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
import Network.Socket
import Network.Socket.ByteString

--- sends a board down the socket
sendBoard :: Socket -> Board -> IO ()
sendBoard s b = sendAll s (serializeBoard b)

--- reads the list of moves from the client
readMoves :: Socket -> IO [Move]
readMoves s = do
  -- TODO: implement this
  return []