-- | Example bot using the PlanetWars module
--
module Main where

import Data.List (maximumBy, partition)
import qualified Data.IntMap as IM
import Data.Ord (comparing)

import PlanetWars

doTurn :: GameState  -- ^ Game state
       -> [Order]    -- ^ Blocks forever
doTurn state = if length myFleets >= numFleets || null myPlanets
                  || null candidates
               -- Do not send anymore out
               then []
               else [Order (planetId strongest) (planetId weakest) ships]
  where
    -- Find my fleets
    (myFleets, enemyFleets) = partition isAllied $ gameStateFleets state

    -- Separate out my planets
    (myPlanets, notMyPlanets) = partition isAllied $ IM.elems $ gameStatePlanets state
    enemyPlanets = filter isHostile notMyPlanets

    -- Common sub-expressions
    sumBy f = sum . map f
    fleetsShips = sumBy fleetShips
    planetsShips = sumBy planetShips

    -- Count my ships and opposing ships
    myShips = fleetsShips myFleets + planetsShips myPlanets
    enemyShips = fleetsShips enemyFleets + planetsShips enemyPlanets

    -- Common sub-expression
    planetsProduction = sumBy planetGrowthRate

    -- Sum my production and opposing production
    myProduction = planetsProduction myPlanets
    enemyProduction = planetsProduction enemyPlanets

    -- Determine fleet count, and attack stance
    (numFleets, attackMode) = if myShips > enemyShips
                              then if winningProduction
                                   then (1, True)
                                   else (3, False)
                              else if winningProduction
                                   then (1, False)
                                   else (5, False)
      where winningProduction = myProduction > enemyProduction

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

    -- Determine where to sent fleets
    candidates = if attackMode then enemyPlanets else notMyPlanets

    -- Find the weakest candidate planet
    weakest = maximumBy (comparing notMyValue) candidates

    -- Send half the planet's ships
    ships = planetShips strongest `div` 2


main :: IO ()
main = bot doTurn
