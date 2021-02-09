module Balance where

--- max distance to a ship/ordnance that a player's ship can see
shipViewRadius :: Double
shipViewRadius = 3

--- max distance away from the sun anything can go before it's 'destroyed'
mapBorder :: Double
mapBorder = 50

--- max delta-v a ship can provide in a normal thrust
thrustDeltaV :: Double
thrustDeltaV = 1

--- max health of any health category
maxHealth :: Int
maxHealth = 6