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

module Main where

import Test.Framework
import qualified TestBoard
import qualified TestMove
import qualified TestOrderValidator

tests :: [Test]
tests = [TestBoard.group, TestMove.group, TestOrderValidator.group]

main :: IO ()
main = defaultMain tests

mainWithOpts :: IO ()
mainWithOpts = do
  let my_test_opts =
        (mempty :: TestOptions)
          { topt_maximum_generated_tests = Just 500
          }
  let my_runner_opts =
        (mempty :: RunnerOptions)
          { ropt_test_options = Just my_test_opts
          }
  defaultMainWithOpts tests my_runner_opts