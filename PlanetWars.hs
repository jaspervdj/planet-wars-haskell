-- | Library for the Planet Wars google ai contest. More information can be
-- found on http://ai-contest.com.
--
module PlanetWars
    ( 
      -- * Data structures
      Resource (..)
    , Planet (..)
    , Fleet (..)
    , Order (..)
    , GameState (..)

      -- * Utility functions
    , isAllied
    , isHostile
    , isNeutral
    , addShips
    , engage
    , engageAll
    , distanceBetween
    , isArrived

      -- * Step the state
    , step

      -- * Communication with the game engine
    , issueOrder
    , finishTurn

      -- * Bots
    , bot
    , ioBot
    ) where

import Control.Applicative ((<$>))
import Data.List (intercalate, isPrefixOf, partition)
import Data.Monoid (Monoid, mempty, mappend)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import System.IO

-- | Class for values that are owned by a player
--
class Resource a where
    owner :: a -> Int

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

instance Resource Planet where
    owner = planetOwner

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

instance Resource Fleet where
    owner = fleetOwner

-- | Representation of an order
--
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
                            (read $ xs !! 4)
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

-- | Check if a given resource is allied
--
isAllied :: Resource r => r -> Bool
isAllied = (== 1) . owner

-- | Check if a given resource is hostile
--
isHostile :: Resource r => r -> Bool
isHostile = (== 2) . owner

-- | Check if a given resource is neutral
--
isNeutral :: Resource r => r -> Bool
isNeutral p = not (isAllied p || isHostile p)

-- | Add (or subtract) a number of ships to (or from) a planet
--
addShips :: Planet  -- ^ Planet to add ships to
         -> Int     -- ^ Number of ships to add
         -> Planet  -- ^ Resulting planet
addShips planet n = planet {planetShips = planetShips planet + n}

-- | Attack the given planet with the given fleet (or reinforce it, when the
-- planet is allied to the fleet)
--
engage :: Planet  -- ^ Planet to engage with
       -> Fleet   -- ^ Fleet to user
       -> Planet  -- ^ Resulting planet
engage planet fleet
    -- Reinforce the planet
    | owner planet == owner fleet = addShips planet $ fleetShips fleet
    -- Attack the planet: planet was conquered
    | shipsAfterAttack < 0 =
        planet {planetShips = -shipsAfterAttack, planetOwner = owner fleet}
    -- Attack failed
    | otherwise = planet {planetShips = shipsAfterAttack}
  where
    shipsAfterAttack = planetShips planet - fleetShips fleet

-- | Apply all fleets in the list to all planets
--
engageAll :: IntMap Planet -> [Fleet] -> IntMap Planet
engageAll planets fleets = foldl engage' planets fleets
  where
    engage' planets' fleet = IM.update (return . flip engage fleet)
                                       (fleetDestination fleet)
                                       planets'

-- | Find the distance between two planets
--
distanceBetween :: Planet -> Planet -> Double
distanceBetween p1 p2 = let dx = planetX p1 - planetX p2
                            dy = planetY p1 - planetY p2
                        in sqrt $ dx * dx + dy * dy

-- | Check if a fleet has arrived
--
isArrived :: Fleet -> Bool
isArrived = (== 0) . fleetTurnsRemaining

-- | Step the game state for one turn
--
step :: GameState -> GameState
step state = state
    { gameStatePlanets = IM.map grow $ engageAll (gameStatePlanets state) ready
    , gameStateFleets = fleets'
    }
  where
    (ready, fleets') =
        partition isArrived $ map stepFleet $ gameStateFleets state
    stepFleet fleet = fleet
        { fleetTurnsRemaining = fleetTurnsRemaining fleet - 1
        }
    grow planet = addShips planet (planetGrowthRate planet)

-- | Execute an order
--
issueOrder :: Order  -- ^ Order to execute
           -> IO ()  -- ^ Result
issueOrder (Order source destination ships) =
    putStrLn $ intercalate " " $ map show [source, destination, ships]

-- | Finish your turn
--
finishTurn :: IO ()   -- ^ Result
finishTurn = do
    putStrLn "go"
    hFlush stdout

-- | Run a deterministic bot
--
bot :: (GameState -> [Order])  -- ^ Deterministic AI function
    -> IO ()                   -- ^ Blocks forever
bot f = ioBot $ mapM_ issueOrder . f

-- | Run an IO bot. This is a more liberal version of 'bot', which allows you to
-- work in the IO monad. However, you need to call 'issueOrder' yourself if you
-- use this function -- 'finishTurn' will still be called automatically.
--
ioBot :: (GameState -> IO ())  -- ^ Bot action
      -> IO ()                 -- ^ Blocks forever
ioBot f = do
    hSetBuffering stdin NoBuffering
    loop mempty
  where
    loop state = do
        line <- takeWhile (/= '#') <$> getLine
        if "go" `isPrefixOf` line
            -- Go Go Go!
            then do
                f state
                finishTurn
                loop mempty
            -- Keep building map
            else loop (buildGameState state line)
