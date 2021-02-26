{-
Copyright 2020 Johann Cooper

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

module Interaction where

import Balance
import Board
import qualified Control.Concurrent.Lock as Lock
import Data.IORef
import Data.Maybe ( fromJust, isJust )
import Graphics.UI.GLUT
import Move
import Network.Socket
import Networking
import Theme
import Util

windowToWorld :: Vec2 -> Vec2
windowToWorld (x, y) = worldPos
  where
    canvasX = (2 * x - fromIntegral windowWidth) / fromIntegral windowWidth
    canvasY = (-2 * y + fromIntegral windowHeight) / fromIntegral windowHeight
    worldPos = (canvasX * mapBorder, canvasY * mapBorder)

isMouseOverShip :: Position -> Entity -> Bool
isMouseOverShip (Position x y) (Ship _ shipPos _ _ _ _ _ _ _ _ _ _) = distance shipPos mouseWorldPos <= hitboxRadius
  where
    mouseWorldPos = windowToWorld (fromIntegral x, fromIntegral y)
    hitboxRadius = 0.5
isMouseOverShip _ _ = False

getClickedShip :: Key -> KeyState -> Position -> Board -> Maybe Entity
getClickedShip (MouseButton LeftButton) Up mousePos board = entity
  where
    entity = if null clickedShips then Nothing else Just (head clickedShips)
    clickedShips = filter (isMouseOverShip mousePos) board
getClickedShip _ _ _ _ = Nothing

getClickedMapPos :: Key -> KeyState -> Position -> Maybe Vec2
getClickedMapPos (MouseButton RightButton) Up (Position x y) = Just (windowToWorld (fromIntegral x, fromIntegral y))
getClickedMapPos _ _ _ = Nothing

handleMoves :: Board -> IORef [Move] -> Lock.Lock -> IORef (Maybe Entity) -> Lock.Lock -> Key -> KeyState -> Position -> IO ()
handleMoves board moveList moveListLock selectedEntity selectedEntityLock key keyState mousePos = do
  let clickedShip = getClickedShip key keyState mousePos board
  let clickedMapPos = getClickedMapPos key keyState mousePos

  Lock.acquire moveListLock
  Lock.acquire selectedEntityLock

  moveList' <- readIORef moveList
  selectedEntity' <- readIORef selectedEntity

  -- -- select new player ship
  -- if isJust clickedShip && (owner (fromJust clickedShip) == 1)
  --   then
  --     writeIORef selectedEntity clickedShip
  --   else
  --     writeIORef selectedEntity selectedEntity'

  -- -- thrust toward board position
  -- if isJust selectedEntity' && isJust clickedMapPos
  --   then
  --     writeIORef moveList (Thrust 1 (vecUnit (fromJust clickedMapPos)):moveList')
  --   else
  --     writeIORef moveList moveList'

  -- -- attack enemy ship
  -- if isJust selectedEntity' && (owner (fromJust clickedShip) == 2)
  --   then
  --     writeIORef moveList (Attack 1 2:moveList')
  --   else
  --     writeIORef moveList moveList'

  Lock.release selectedEntityLock
  Lock.release moveListLock

sendMovesToServer :: Socket -> IORef Board -> Lock.Lock -> IORef [Move] -> Lock.Lock -> Key -> KeyState -> IO ()
sendMovesToServer socket board boardLock moveList moveListLock (SpecialKey KeyUp) Up = do
  Lock.acquire moveListLock
  moveList' <- readIORef moveList
  sendMoves socket moveList'
  writeIORef moveList []
  print "sent"
  -- updateBoardState socket board boardLock
  print "board updated"
  Lock.release moveListLock
sendMovesToServer _ _ _ _ _ _ _ = return ()

updateBoardState :: Socket -> IORef Board -> Lock.Lock -> IO ()
updateBoardState socket board boardLock = do
  Lock.acquire boardLock
  board' <- readBoard socket
  writeIORef board board'
  Lock.release boardLock