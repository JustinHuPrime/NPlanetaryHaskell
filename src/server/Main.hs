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

import Board
import Control.Concurrent
import qualified Control.Concurrent.Lock as Lock
import qualified Control.Exception as E
import Control.Monad
import Data.IORef
import Engine
import Move
import NetInterface
import Network.Socket
import Networking
import System.Environment
import System.Exit
import Text.Read

{-
Concurrency system:
A thread reads moves, and filters out the bad ones
It then locks the moveListLock, and adds the moves to the list
If the list is less than `numPlayers` long:
  releases the moveListLock and downs the newBoardSem by one - this thread is not the leader this time around
  once the wait is done, it sends out the new board state and ups on the sentBoardSem
  repeat.
If the list is `numPlayers` long:
  it processes the moves in the list and saves the new board
  it ups the newBoardSem by `numPlayers` - 1, and it sends out the new board state
  it downs the sentBoardSem by `numPlayers` - 1, and it releases the moveListLock
  repeat.
-}

handler :: Socket -> IORef Board -> QSemN -> QSemN -> IORef [[Move]] -> Lock.Lock -> Int -> Int -> IO ()
handler s board newBoardSem sentBoardSem moveList moveListLock numPlayers playerId = do
  -- send out initial board state
  initBoard <- readIORef board
  sendBoard s initBoard
  forever
    ( do
        -- get list of moves
        b <- readIORef board
        moves <- validateMoves b playerId <$> readMoves s b
        Lock.acquire moveListLock
        ml <- readIORef moveList
        writeIORef moveList (moves : ml)
        if length moves == numPlayers
          then
            let updated = updateBoard b ml
             in do
                  writeIORef board updated
                  signalQSemN newBoardSem (numPlayers - 1)
                  sendBoard s updated
                  waitQSemN sentBoardSem (numPlayers - 1)
                  Lock.release moveListLock
          else do
            Lock.release moveListLock
            waitQSemN newBoardSem 1
            updated <- readIORef board
            sendBoard s updated
            signalQSemN sentBoardSem 1
    )

loop :: Int -> Int -> IORef Board -> QSemN -> QSemN -> IORef [[Move]] -> Lock.Lock -> Socket -> IO ()
loop 1 numPlayers board newBoardSem sentBoardSem moveList moveListLock s = do
  (conn, _) <- accept s
  handler conn board newBoardSem sentBoardSem moveList moveListLock numPlayers 1
  gracefulClose conn 5000
loop currentPlayer numPlayers board newBoardSem sentBoardSem moveList moveListLock s = do
  (conn, _) <- accept s
  void (forkFinally (handler conn board newBoardSem sentBoardSem moveList moveListLock numPlayers currentPlayer) (const (gracefulClose conn 5000)))
  loop (currentPlayer - 1) numPlayers board newBoardSem sentBoardSem moveList moveListLock s

server :: Int -> IO ()
server numPlayers = do
  board <- newIORef [] :: IO (IORef Board)
  newBoardSem <- newQSemN 0
  sentBoardSem <- newQSemN 0
  moveList <- newIORef [] :: IO (IORef [[Move]])
  moveListLock <- Lock.new
  addr <- resolve
  E.bracket (open addr) close (loop numPlayers numPlayers board newBoardSem sentBoardSem moveList moveListLock)
  exitSuccess
  where
    resolve =
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
        | numPlayers > 4 -> do
          putStrLn "Number of players must be less than four"
          exitWith (ExitFailure 1)
        | otherwise -> do
          putStrLn "N-Planetary server version 0.1.0"
          server numPlayers
      Nothing -> do
        putStrLn "Could not parse the number of players"
        exitWith (ExitFailure 1)

-- get arguments, expecting one and only arg to be number of players
-- open server, initialize world state
-- when player connects, remember that and assign them a colour out of [Red, Green, Blue, Yellow]
-- wait for packet from all players
-- update world state, send update packet out