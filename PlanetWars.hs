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
    , engage
    , distanceBetween

      -- * Communication with the game engine
    , issueOrder
    , finishTurn

      -- * Bots
    , bot
    , ioBot
    ) where

import Control.Applicative ((<$>))
import Data.List (intercalate, isPrefixOf)
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

-- | Check that fleet is arrived
--
fleetIsArrived :: Fleet -> Bool
fleetIsArrived = (<=0) . fleetTurnsRemaining

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

-- | Find planet ib GameState
--
getPlanetById :: Int -> GameState -> Planet
getPlanetById id state = (IM.!) (gameStatePlanets state) id

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
isHostile = (> 1) . owner

-- | Check if a given resource is neutral
--
isNeutral :: Resource r => r -> Bool
isNeutral = (== 0) . owner


-- | Attack the given planet with the given fleet (or reinforce it, when the
-- planet is allied to the fleet)
--
engage :: Planet  -- ^ Planet to engage with
       -> Fleet   -- ^ Fleet to user
       -> Planet  -- ^ Resulting planet
engage planet fleet
    -- Reinforce the planet
    | owner planet == owner fleet =
        planet {planetShips = planetShips planet + fleetShips fleet}
    -- Attack the planet: planet was conquered
    | shipsAfterAttack < 0 =
        planet {planetShips = -shipsAfterAttack, planetOwner = owner fleet}
    -- Attack failed
    | otherwise = planet {planetShips = shipsAfterAttack}
  where
    shipsAfterAttack = planetShips planet - fleetShips fleet

-- | Attack the given planet with several fleets
-- The algorithm is compatible with this proposition: http://ai-contest.com/forum/viewtopic.php?f=18&t=419
-- TODO: implement the original algorithm
--
engageMany :: Planet -> [Fleet] -> Planet 
engageMany planet fleets =
    fight planet (combine (extractFleet planet) fleets)
    where
        extractFleet planet = IM.singleton (owner planet) (planetShips planet)

        combine fleets [] = fleets
        combine fleets (f:rest) = combine (IM.insertWith (+) (owner f) (fleetShips f) fleets) rest

        fight planet fleets
            -- if no fleets left
            | IM.null fleets =
                planet {planetShips = 0}
            -- if the only fleet left
            | IM.size fleets == 1 =
                let (o,f) = head $ IM.assocs fleets
                in planet {planetOwner=o, planetShips=f}
            | otherwise =
                -- find the smallest fleet and reduce all fleets by its size
                let minShips = minimum $ IM.elems fleets
                    fleets' = IM.filter (>0) $ IM.map (+ (-minShips)) $ fleets
                -- and go to the next round
                in fight planet fleets'

-- | Find the distance between two planets
--
distanceBetween :: Planet -> Planet -> Int
distanceBetween p1 p2 = let dx = planetX p1 - planetX p2
                            dy = planetY p1 - planetY p2
                            dist = sqrt $ dx * dx + dy * dy
                        in ceiling dist

-- | Process order - create a new fleet, does nothing if order is impossible
--
processOrder :: Order -> GameState -> GameState
processOrder order state =
    let planetSrc = getPlanetById (orderSource order) state
        planetDst = getPlanetById (orderDestination order) state
        player = planetOwner planetSrc
        ships = orderShips order
    in
        if (isNeutral planetSrc) && ((planetShips planetSrc) < ships)
            then state
            else 
                 let planets' = IM.insert (orderSource order) planetSrc{planetShips = (planetShips planetSrc)-ships} (gameStatePlanets state)
                     newFleet = Fleet player ships (orderSource order) (orderDestination order) dist dist
                     dist = distanceBetween planetSrc planetDst
                     fleets'  = newFleet : (gameStateFleets state)
                 in GameState planets' fleets'

-- | Process a list of orders
--
processOrders :: [Order] -> GameState -> GameState
processOrders = flip $ foldr $ processOrder

-- | Process one tick of timer: planets are growing and fleets are moving
--
processTick :: GameState -> GameState
processTick state = GameState (IM.map grow1 (gameStatePlanets state)) (map move1 (gameStateFleets state))
    where
        grow1 planet 
            | isNeutral planet = planet
            | otherwise = planet { planetShips = (planetShips planet + planetGrowthRate planet) }
        move1 fleet = fleet { fleetTurnsRemaining = (fleetTurnsRemaining fleet - 1) }

-- | Aux
partitionToIntMap :: (a -> Int) -> [a] -> IntMap [a]
partitionToIntMap fn as =
    let ins x = IM.insertWith (++) (fn x) [x]
    in  foldr ins IM.empty as

-- | Do all fights
--
fightAll :: GameState -> GameState
fightAll state = 
    let arrivedFleets = filter fleetIsArrived (gameStateFleets state)
        fleets' = filter (not . fleetIsArrived) (gameStateFleets state)
        planets' = IM.map fightOverPlanet (gameStatePlanets state)
            where 
                fightOverPlanet planet =
                    engageMany planet (filter ((== planetId planet) . fleetDestination) arrivedFleets)
    in GameState planets' fleets'

-- | Simulate one step of a model
--
oneStep :: [Order] -> GameState -> GameState
oneStep orders = fightAll . processTick . (processOrders orders)


-- | Issue an order
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
