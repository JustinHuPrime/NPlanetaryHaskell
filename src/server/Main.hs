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

import Control.Concurrent
import qualified Control.Exception as E
import Control.Monad
import qualified Data.ByteString.Char8 as C
import NetInterface
import Network.Socket
import Network.Socket.ByteString
import System.Environment
import System.Exit
import Text.Read

handler :: Socket -> IO ()
handler c = do
  msg <- recv c 1024
  sendAll c msg
  C.putStrLn msg

loop :: Int -> Socket -> IO ()
loop 0 s = do
  return ()
loop numPlayers s = do
  (conn, _) <- accept s
  void (forkFinally (handler conn) (const (gracefulClose conn 5000)))
  loop (numPlayers - 1) s

server :: Int -> IO ()
server numPlayers = do
  addr <- resolve
  E.bracket (open addr) close (loop numPlayers)
  exitSuccess
  where
    resolve = do
      let hints =
            defaultHints
              { addrFlags = [AI_PASSIVE],
                addrSocketType = Stream
              }
       in head <$> getAddrInfo (Just hints) Nothing (Just port)
    open addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      setSocketOption sock ReuseAddr 1
      withFdSocket sock setCloseOnExecIfNeeded
      bind sock (addrAddress addr)
      listen sock 1024
      return sock

main :: IO ()
main = do
  args <- getArgs
  if length args /= 1
    then do
      putStrLn "Expected one argument - the number of players"
      exitWith (ExitFailure 1)
    else case (readMaybe (head args) :: Maybe Int) of
      Just numPlayers
        | numPlayers <= 1 -> do
          putStrLn "Number of players must be at least two"
          exitWith (ExitFailure 1)
        | otherwise -> do
          server numPlayers
      Nothing -> do
        putStrLn "Could not parse the number of players"
        exitWith (ExitFailure 1)

-- get arguments, expecting one and only arg to be number of players
-- open server, initialize world state
-- when player connects, remember that and assign them a colour out of [Red, Green, Blue, Yellow]
-- wait for packet from all players
-- update world state, send update packet out