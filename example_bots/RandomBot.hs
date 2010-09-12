-- | Example bot using the PlanetWars module
--
module Main where

import Control.Monad (when, unless)
import Data.List (maximumBy, minimumBy, partition)
import qualified Data.IntMap as IM
import Data.Ord (comparing)
import Random (randomRIO)

import PlanetWars

doTurn :: GameState  -- ^ Game state
       -> IO ()      -- ^ Blocks forever
doTurn state = when (null myFleets) $
    unless (null myPlanets) $ do
    -- Random AI
    ndxSrc <- randomRIO (0, length myPlanets - 1)
    ndxDest <- randomRIO (0, length planets - 1)
    let
        source = myPlanets !! ndxSrc
        destination = planets !! ndxDest
        ships = planetShips source `div` 2
    issueOrder $ Order (planetId source) (planetId destination) ships

  where
    myFleets = filter isAllied $ gameStateFleets state

    -- Retrieve all planets
    planets = map snd $ IM.toList $ gameStatePlanets state

    -- Filter my planets
    myPlanets = filter isAllied $ planets

main :: IO ()
main = ioBot doTurn
