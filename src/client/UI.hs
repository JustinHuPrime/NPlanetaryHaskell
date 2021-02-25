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

module UI where

import Board
import qualified Control.Concurrent.Lock as Lock
import Data.IORef
import Graphics.UI.GLUT
import Move
import Renderer

--- display the window
display :: IORef Board -> Lock.Lock -> DisplayCallback
display board boardLock = do
  clearColor $= Color4 1 1 1 1
  clear [ColorBuffer]

  Lock.acquire boardLock
  board' <- readIORef board
  Lock.release boardLock

  renderBoard board'

  swapBuffers

--- handles window resizing
reshape :: ReshapeCallback
reshape size = do
  viewport $= (Position 0 0, size)
  postRedisplay Nothing

--- handles keyboard and mouse events
keyboardMouse :: IORef Board -> Lock.Lock -> IORef [Move] -> Lock.Lock -> KeyboardMouseCallback
keyboardMouse board boardLock moveList moveListLock pressed state modifiers position = do
  -- TODO: write this
  -- probably want to look at changing the cursor to avoid needing to render text?
  return ()

--- handles idling
idle :: IORef Board -> Lock.Lock -> IdleCallback
idle board boardLock = do
  -- TODO: remove if unneeded. If anything changes with the board, add postRedisplay Nothing to it.
  return ()