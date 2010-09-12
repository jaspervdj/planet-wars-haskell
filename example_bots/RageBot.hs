-- | Example bot using the PlanetWars module
--
module Main where

import Control.Monad (guard)
import Data.List (maximumBy, minimumBy, partition)
import qualified Data.IntMap as IM
import Data.Ord (comparing)

import PlanetWars

doTurn :: GameState  -- ^ Game state
       -> [Order]    -- ^ Orders
doTurn state = do
    guard $ not $ null enemyPlanets
    p <- myPlanets
    let ships = planetShips p
    guard $ ships >= 10 * planetGrowthRate p
    let closest = minimumBy (comparing $ distanceBetween p) enemyPlanets
    return $ Order (planetId p) (planetId closest) ships
  where
    -- Partition all planets
    (myPlanets, notMyPlanets) = partition isAllied $
        map snd $ IM.toList $ gameStatePlanets state
    enemyPlanets = filter isHostile notMyPlanets

main :: IO ()
main = bot doTurn
