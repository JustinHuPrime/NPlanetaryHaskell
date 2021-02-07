module Balance where

--- max distance to an ordnance a ship can be before it's hit (natural units)
ordnanceRadius :: Double
ordnanceRadius = 1

--- blast radius of a nuke
nukeRadius :: Double
nukeRadius = 1

--- max distance to a ship/ordnance that a player's ship can see
shipViewRadius :: Double
shipViewRadius = 3

--- max distance to a ship/ordnance that a player's base can see
baseViewRadius :: Double 
baseViewRadius = 5

--- max distance away from the sun anything can go before it's 'destroyed'
mapBorder :: Double 
mapBorder = 50