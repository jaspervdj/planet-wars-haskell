-- | Example bot using the PlanetWars module
--
module Main where

import Data.List (maximumBy, partition)
import qualified Data.IntMap as IM
import Data.Ord (comparing)

import PlanetWars

doTurn :: GameState  -- ^ Game state
       -> [Order]    -- ^ Blocks forever
doTurn state = if null myFleets && (not . null) myPlanets
                  && (not . null) notMyPlanets
               then [Order (planetId strongest) (planetId weakest) ships]
               else []
  where
    -- Find my fleets
    myFleets = filter isAllied $ gameStateFleets state

    -- Separate out my planets
    (myPlanets, notMyPlanets) = partition isAllied $ IM.elems $ gameStatePlanets state

    -- Conversion function
    intToDouble :: Int -> Double
    intToDouble = fromInteger . toInteger

    -- Sub-expressions
    growthFactor p = intToDouble $ 1 + planetGrowthRate p
    shipsFactor p = intToDouble $ planetShips p

    -- Value of my planets
    myValue p = shipsFactor p / growthFactor p

    -- Value of other planets
    notMyValue p = growthFactor p / shipsFactor p

    -- Find my strongest planet
    strongest = maximumBy (comparing myValue) myPlanets

    -- Find the weakest planet I don't control
    weakest = maximumBy (comparing notMyValue) notMyPlanets

    -- Send half the planet's ships
    ships = planetShips strongest `div` 2


main :: IO ()
main = bot doTurn
