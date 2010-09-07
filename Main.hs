-- | Example bot using the PlanetWars module
--
module Main where

import Data.List (maximumBy, minimumBy)
import qualified Data.IntMap as IM
import Data.Ord (comparing)

import PlanetWars

doTurn :: GameState  -- ^ Game state
       -> [Order]    -- ^ Orders
doTurn state = if (null $ gameStateFleets state)
    -- Simple ai
    then [Order (planetId strongest) (planetId weakest) ships]
    -- If we have a fleet in flight, just do nothing
    else []
  where
    strongest = maximumBy (comparing planetShips)
              $ filter isAlliedPlanet
              $ map snd $ IM.toList $ gameStatePlanets state
    weakest = minimumBy (comparing planetShips)
              $ filter (not . isAlliedPlanet)
              $ map snd $ IM.toList $ gameStatePlanets state
    ships = planetShips strongest `div` 2

main :: IO ()
main = bot doTurn
