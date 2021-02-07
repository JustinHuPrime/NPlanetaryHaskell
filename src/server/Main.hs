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

module Main (main) where

import Network.Socket
import System.Environment
import System.Exit
import Text.Read

main = do
  args <- getArgs
  if length args /= 1 then do
    putStrLn "Expected one argument - the number of players"
    exitWith (ExitFailure 1)
  else do
    case (readMaybe (head args) :: Maybe Int) of
      Just numPlayers | numPlayers <= 1 -> do
        putStrLn "Number of players must be at least two"
        exitWith (ExitFailure 1)
                      | otherwise -> exitSuccess
      Nothing -> do
        putStrLn "Could not parse the number of players"
        exitWith (ExitFailure 1)
  -- get arguments, expecting one and only arg to be number of players
  -- open server, initialize world state
  -- when player connects, remember that and assign them a colour out of [Red, Green, Blue, Yellow]
  -- wait for packet from all players
  -- update world state, send update packet out