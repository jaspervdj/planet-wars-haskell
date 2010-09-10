-- | Example bot using the PlanetWars module
--
module Main where

import Data.List (maximumBy, minimumBy, partition)
import qualified Data.IntMap as IM
import Data.Ord (comparing)

import PlanetWars

doTurn :: GameState  -- ^ Game state
       -> [Order]    -- ^ Orders
doTurn state = if null myFleets
    -- Simple ai
    then [Order (planetId strongest) (planetId weakest) ships]
    -- If we have a fleet in flight, just do nothing
    else []
  where
    myFleets = filter isAllied $ gameStateFleets state

    -- Partition all planets
    (myPlanets, notMyPlanets) = partition isAllied $
        map snd $ IM.toList $ gameStatePlanets state

    -- Find our strongest planet and the weakest neutral/hostile planet
    strongest = maximumBy (comparing planetShips) myPlanets
    weakest = minimumBy (comparing planetShips) notMyPlanets

    -- Select half of the ships
    ships = planetShips strongest `div` 2

main :: IO ()
main = bot doTurn
