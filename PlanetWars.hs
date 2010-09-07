-- | Library for the Planet Wars google ai contest. More information can be
-- found on http://ai-contest.com.
--
module PlanetWars
    ( 
      -- * Data structures
      Planet (..)
    , Fleet (..)
    , Order (..)
    , GameState (..)

      -- * Utility functions
    , isAlliedPlanet
    , isHostilePlanet
    , isNeutralPlanet
    , engage

      -- * Communication with the game engine
    , issueOrder
    , finnishTurn

      -- * Bots
    , bot
    ) where

import Control.Applicative ((<$>))
import Data.List (intercalate, isPrefixOf)
import Data.Monoid (Monoid, mempty, mappend)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import System.IO

-- | Representation of a planet
--
data Planet = Planet
    { planetId         :: Int
    , planetOwner      :: Int
    , planetShips      :: Int
    , planetGrowthRate :: Int
    , planetX          :: Double
    , planetY          :: Double
    } deriving (Show)

-- | Representation of a fleet
--
data Fleet = Fleet
    { fleetOwner          :: Int
    , fleetShips          :: Int
    , fleetSource         :: Int
    , fleetDestination    :: Int
    , fleetTripLength     :: Int
    , fleetTurnsRemaining :: Int
    } deriving (Show)

-- | Representation of an order
data Order = Order
    { orderSource      :: Int
    , orderDestination :: Int
    , orderShips       :: Int
    } deriving (Show)

-- | A data structure describing the game state
--
data GameState = GameState
    { gameStatePlanets :: IntMap Planet
    , gameStateFleets  :: [Fleet]
    } deriving (Show)

instance Monoid GameState where
    mempty = GameState mempty mempty
    mappend (GameState p1 f1) (GameState p2 f2) =
        GameState (p1 `mappend` p2) (f1 `mappend` f2)

-- | Auxiliary function for parsing the game state. This function takes an
-- initial state, and a line. The line is parsed and content is applied on the
-- given state. Folding with this function can produce the entire game state.
--
buildGameState :: GameState  -- ^ Initial game state
               -> String     -- ^ Line to parse and apply
               -> GameState  -- ^ Resulting game state
buildGameState state string = case words string of
    ("P" : xs) ->
        let planet = Planet planetId'
                            (read $ xs !! 2)
                            (read $ xs !! 3)
                            (read $ xs !! 5)
                            (read $ xs !! 0)
                            (read $ xs !! 1)
        in state { gameStatePlanets = IM.insert planetId' planet
                                                (gameStatePlanets state)
                 }
    ("F" : xs) ->
        let fleet = Fleet (read $ xs !! 0)
                          (read $ xs !! 1)
                          (read $ xs !! 2)
                          (read $ xs !! 3)
                          (read $ xs !! 4)
                          (read $ xs !! 5)
        in state { gameStateFleets = fleet : gameStateFleets state
                 }
    _ -> state
  where
    planetId' = IM.size $ gameStatePlanets state

-- | Check if a given planet is allied
--
isAlliedPlanet :: Planet -> Bool
isAlliedPlanet = (== 1) . planetOwner

-- | Check if a given planet is hostile
--
isHostilePlanet :: Planet -> Bool
isHostilePlanet = (== 2) . planetOwner

-- | Check if a given planet is neutral
--
isNeutralPlanet :: Planet -> Bool
isNeutralPlanet p = not (isAlliedPlanet p || isHostilePlanet p)

-- | Attack the given planet with the given fleet (or reinforce it, when the
-- planet is allied to the fleet)
--
engage :: Planet  -- ^ Planet to engage with
       -> Fleet   -- ^ Fleet to user
       -> Planet  -- ^ Resulting planet
engage planet fleet
    -- Reinforce the planet
    | planetOwner planet == fleetOwner fleet =
        planet {planetShips = planetShips planet + fleetShips fleet}
    -- Attack the planet: planet was conquered
    | shipsAfterAttack < 0 =
        planet {planetShips = -shipsAfterAttack, planetOwner = fleetOwner fleet}
    -- Attack failed
    | otherwise = planet {planetShips = shipsAfterAttack}
  where
    shipsAfterAttack = planetShips planet - fleetShips fleet

-- | Execute an order
--
issueOrder :: Order  -- ^ Order to execute
           -> IO ()  -- ^ Result
issueOrder (Order source destination ships) =
    putStrLn $ intercalate " " $ map show [source, destination, ships]

-- | Finnish your turn
--
finnishTurn :: IO ()   -- ^ Result
finnishTurn = do
    putStrLn "go"
    hFlush stdout

-- | Run a deterministic bot
--
bot :: (GameState -> [Order])  -- ^ Deterministic AI function
    -> IO ()                   -- ^ Blocks forever
bot f = do
    hSetBuffering stdin NoBuffering
    loop mempty
  where
    loop state = do
        line <- takeWhile (/= '#') <$> getLine
        if "go" `isPrefixOf` line
            -- Go Go Go!
            then do
                mapM_ issueOrder $ f state
                finnishTurn
                loop mempty
            -- Keep building map
            else loop (buildGameState state line)
