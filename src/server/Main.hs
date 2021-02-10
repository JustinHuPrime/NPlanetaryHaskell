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
  board' <- readIORef board
  sendBoard s (filterVisible board' playerId)
  forever
    ( do
        -- get list of moves
        board' <- readIORef board
        moves <- validateMoves board' playerId <$> readMoves s
        Lock.acquire moveListLock
        ml <- readIORef moveList
        writeIORef moveList (moves : ml)
        if length moves == numPlayers
          then do
            updated <- updateBoard (concat ml) board'
            writeIORef board updated
            signalQSemN newBoardSem (numPlayers - 1)
            sendBoard s (filterVisible updated playerId)
            waitQSemN sentBoardSem (numPlayers - 1)
            Lock.release moveListLock
          else do
            Lock.release moveListLock
            waitQSemN newBoardSem 1
            updated <- readIORef board
            sendBoard s (filterVisible updated playerId)
            signalQSemN sentBoardSem 1
    )

acceptConns :: Int -> Int -> IORef Board -> QSemN -> QSemN -> IORef [[Move]] -> Lock.Lock -> Socket -> IO ()
acceptConns 1 numPlayers board newBoardSem sentBoardSem moveList moveListLock s = do
  (conn, _) <- accept s
  handler conn board newBoardSem sentBoardSem moveList moveListLock numPlayers 1
  gracefulClose conn 5000
acceptConns currentPlayer numPlayers board newBoardSem sentBoardSem moveList moveListLock s = do
  (conn, _) <- accept s
  void (forkFinally (handler conn board newBoardSem sentBoardSem moveList moveListLock numPlayers currentPlayer) (const (gracefulClose conn 5000)))
  acceptConns (currentPlayer - 1) numPlayers board newBoardSem sentBoardSem moveList moveListLock s

server :: Int -> IO ()
server numPlayers = do
  board <- newIORef [] :: IO (IORef Board)
  newBoardSem <- newQSemN 0
  sentBoardSem <- newQSemN 0
  moveList <- newIORef [] :: IO (IORef [[Move]])
  moveListLock <- Lock.new
  addr <- resolve
  E.bracket (open addr) close (acceptConns numPlayers numPlayers board newBoardSem sentBoardSem moveList moveListLock)
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
      s <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      setSocketOption s ReuseAddr 1
      withFdSocket s setCloseOnExecIfNeeded
      bind s (addrAddress addr)
      listen s 1024
      return s

main :: IO ()
main = do
  args <- getArgs
  if length args /= 1
    then do
      putStrLn "Expected one argument - the number of players"
      exitWith (ExitFailure 1)
    else case (readMaybe (head args) :: Maybe Int) of
      Just numPlayers
        | numPlayers /= 2 -> do
          putStrLn "Number of players must be exactly two"
          exitWith (ExitFailure 1)
        | otherwise -> do
          putStrLn "N-Planetary server version 0.1.0"
          server numPlayers
      Nothing -> do
        putStrLn "Could not parse the number of players"
        exitWith (ExitFailure 1)