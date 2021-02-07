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
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Board
import qualified Control.Concurrent.Lock as Lock
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import Data.IORef
import Move
import NetInterface
import Network.Socket
import Network.Socket.ByteString
import System.Environment
import System.Exit

loop :: IORef Board -> IORef [Move] -> Lock.Lock -> Socket -> IO ()
loop board moveList lock s = do
  sendAll s "Hello!"
  msg <- recv s 1024
  C.putStrLn msg
  _ <- recv s 1024
  return ()

client :: String -> IO ()
client serverAddr = do
  board <- newIORef [] :: IO (IORef Board)
  moveList <- newIORef [] :: IO (IORef [Move])
  lock <- Lock.new
  addr <- resolve serverAddr
  E.bracket (open addr) close (loop board moveList lock)
  exitSuccess
  where
    resolve addr = do
      head <$> getAddrInfo (Just (defaultHints {addrSocketType = Stream})) (Just addr) (Just port)
    open addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      connect sock (addrAddress addr)
      return sock

main :: IO ()
main = do
  args <- getArgs
  if length args /= 1
    then do
      putStrLn "Expected one argument - the address of the server"
      exitWith (ExitFailure 1)
    else do
      putStrLn "N-Planetary client version 0.1.0"
      client (head args)

-- get arguments, expecting one and only arg to be server address
-- connect to server, get world state
-- open gui window, display world state, on mouse, change world state (!), eventually send world state
-- get world state, repeat previous