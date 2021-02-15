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

module Serializing where

import qualified Data.ByteString.Char8 as B
import Text.Read

--- serializes an group's list of bytestrings (uses group separators)
serializeGroupList :: [B.ByteString] -> B.ByteString
serializeGroupList = B.intercalate (B.singleton '\x1D')

--- serializes an integer as a bytestring
serializeInt :: Int -> B.ByteString
serializeInt = B.pack . show

--- parses an integer as a bytestring
parseInt :: String -> Maybe Int
parseInt s = readMaybe s :: Maybe Int

--- serializes a double as a bytestring
serializeDouble :: Double -> B.ByteString
serializeDouble x = B.pack (show (round (x * fixedPointPrecision) :: Int))

--- parses a double as a bytestring
parseDouble :: String -> Maybe Double
parseDouble s = do
  fixedPoint <- parseInt s
  return (fromIntegral fixedPoint / fixedPointPrecision)

--- precision for fixed point numbers
fixedPointPrecision :: Double
fixedPointPrecision = 10000

--- serializes a boolean into a bytestring
serializeBool :: Bool -> B.ByteString
serializeBool True = B.singleton 'T'
serializeBool False = B.singleton 'F'

--- parses a boolean from a string
parseBool :: String -> Maybe Bool
parseBool "T" = Just True
parseBool "F" = Just False
parseBool _ = Nothing