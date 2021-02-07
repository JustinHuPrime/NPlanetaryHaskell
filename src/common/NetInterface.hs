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

module NetInterface where

import qualified Data.ByteString.Char8 as B
import Network.Socket
import Network.Socket.ByteString

port :: [Char]
port = "3000"

readPacket :: Socket -> IO B.ByteString
readPacket s = do
  str <- recv s 4096
  if '\x03' `B.elem` str
    then do
      return (B.takeWhile (/= '\x03') str)
    else do
      B.append str <$> readPacket s