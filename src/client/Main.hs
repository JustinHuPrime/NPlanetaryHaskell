{-
Copyright 2020 Justin Hu, Johann Cooper

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
import qualified Control.Concurrent.Lock as Lock
import qualified Control.Exception as E
import Data.IORef
import Graphics.UI.GLUT
import Move
import NetInterface
import Network.Socket
import Networking
import System.Exit
import Theme
import UI

openWindow :: IORef Board -> Lock.Lock -> IORef [Move] -> Lock.Lock -> Socket -> IO ()
openWindow board boardLock moveList moveListLock s = do
  board' <- readBoard s
  writeIORef board board'

  -- TODO: are there any additional options we need to pass?
  initialDisplayMode $= [DoubleBuffered]
  -- TODO: probably need to setup openGL versions and such
  _ <- createWindow "N-Planetary"
  windowSize $= Size (fromIntegral windowWidth) (fromIntegral windowHeight)

  displayCallback $= display board boardLock
  -- TODO: add more input callbacks as needed
  keyboardMouseCallback $= Just (keyboardMouse board boardLock moveList moveListLock)
  idleCallback $= Just (idle board boardLock)
  closeCallback $= Just (close s)

  mainLoop

initBoard :: Board
initBoard = [
  Ship 0 (  0,   0) (0, 0) 2 "test_0" 0 False 0 0 0 0 0,
  Ship 1 ( 25,  25) (0, 0) 2 "test_1" 0 False 0 0 0 0 0,
  Ship 2 (-25, -25) (0, 0) 2 "test_2" 0 False 0 0 0 0 0
  ]

client :: String -> IO ()
client serverAddr = do
  board <- newIORef [] :: IO (IORef Board)
  boardLock <- Lock.new
  moveList <- newIORef [] :: IO (IORef [Move])
  moveListLock <- Lock.new
  addr <- resolve serverAddr
  E.bracket (open addr) close (openWindow board boardLock moveList moveListLock)
  exitSuccess
  where
    resolve addr = do
      head <$> getAddrInfo (Just (defaultHints {addrSocketType = Stream})) (Just addr) (Just port)
    open addr = do
      s <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      connect s (addrAddress addr)
      return s

main :: IO ()
main = do
  (_, args) <- getArgsAndInitialize
  if length args /= 1
    then do
      putStrLn "Expected one argument - the address of the server"
      exitWith (ExitFailure 1)
    else do
      putStrLn "N-Planetary client version 0.1.0"
      client (head args)