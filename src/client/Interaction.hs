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
import Data.Maybe
import Graphics.UI.GLUT
import Move
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

handleInput :: Board -> [Move] -> Key -> KeyState -> Position -> IO [Move]
handleInput board moveList key keyState mousePos = do
  let clickedShip = getClickedShip key keyState mousePos board
  print (isJust clickedShip)

  let moves = [Thrust (Board.idNum (fromJust clickedShip)) (0, 1) | isJust clickedShip]
  -- let moves = [Thrust 2 (0, 1) | isJust clickedShip]

  let clickedMapPos = getClickedMapPos key keyState mousePos



  return moves