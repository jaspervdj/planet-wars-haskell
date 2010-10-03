-- | Library for the Planet Wars google ai contest. More information can be
-- found on http://ai-contest.com.
--
{-# LANGUAGE FlexibleInstances #-}
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
    , engageAll
    , fleetIsArrived
    , getPlanetById
    , distanceBetween
    , centroid
    , isArrived
    , planets
    , production
    , planetById
    , willSurviveAttack
    , currentOwner
    , planetsUnderAttack
    , incomingFleets

      -- * Step the state
    , step
    , stepAllFleets

      -- * Simulation
    , engage
    , engageMany
    , turnsBetween
    , modelStep

      -- * Communication with the game engine
    , issueOrder
    , finishTurn

      -- * Bots
    , bot
    , ioBot
    , debugBot

      -- * Debugging
    , stateFromFile
    , unique
    ) where

import Control.Applicative ((<$>))
import Data.List (intercalate, isPrefixOf, partition, foldl', sortBy)
import Data.Maybe (fromJust)
import Data.Monoid (Monoid, mempty, mappend)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.Ord (comparing)
import System.IO
import System.IO.Error (isEOFError)

-- | Class for values that are owned by a player
--
class Resource a where
    owner :: a -> Int

-- | Class for physical entities
--
class Entity a where
    getX :: a -> Double
    getY :: a -> Double

instance Entity (Double, Double) where
    getX = fst
    getY = snd

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

instance Entity Planet where
    getX = planetX
    getY = planetY

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

-- | A data structure describing the game state.
--
-- * Planets are mapped by id
--
-- * Fleets are mapped by destination
--
data GameState = GameState
    { gameStatePlanets :: IntMap Planet
    , gameStateFleets  :: [Fleet]
    } deriving (Show)

instance Monoid GameState where
    mempty = GameState mempty mempty
    mappend (GameState p1 f1) (GameState p2 f2) =
        GameState (p1 `mappend` p2) (f1 `mappend` f2)

-- | Find planet in GameState with given planetId
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
isNeutral = (<= 0) . owner

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
distanceBetween :: (Entity a, Entity b) => a -> b -> Double
distanceBetween p1 p2 = let dx = getX p1 - getX p2
                            dy = getY p1 - getY p2
                        in sqrt $ dx * dx + dy * dy

-- | Find the centroid of the given planets
--
centroid :: IntMap Planet -> (Double, Double)
centroid planets = div' $ IM.fold add' (0, 0) planets
  where
    add' planet (x, y) = (x + planetX planet, y + planetY planet)
    div' (x, y) = let size = fromIntegral $ IM.size planets
                  in (x / size, y / size)

-- | Check if a fleet has arrived
--
isArrived :: Fleet -> Bool
isArrived = (== 0) . fleetTurnsRemaining

-- | List of Planets from a game state.
--
planets :: GameState  -- ^ Game state to analyze
        -> [Planet]   -- ^ List of Planets
planets state = map snd $ IM.toList $ gameStatePlanets state

-- | Calculate the production (number of new ships in the next turn) of both
-- players.
--
production :: GameState  -- ^ Game state to analyze
           -> (Int, Int) -- ^ Pair having the player and enemy's production
production g = foldl' prod (0,0) (planets g)
  where 
    prod (x,y) p = case planetOwner p of
      0 -> (x,y)
      1 -> (x + planetGrowthRate p, y)
      2 -> (x, y + planetGrowthRate p)

-- | Get a planet by ID. Make sure the ID exists!
--
planetById :: GameState -> Int -> Planet
planetById state id' = fromJust $ IM.lookup id' $ gameStatePlanets state

-- | Step the game state for one turn
--
step :: GameState -> GameState
step state = state
    { gameStatePlanets = engageAll (IM.map grow $ gameStatePlanets state) ready
    , gameStateFleets = fleets'
    }
  where
    (ready, fleets') =
        partition isArrived $ map stepFleet $ gameStateFleets state
    stepFleet fleet = fleet
        { fleetTurnsRemaining = fleetTurnsRemaining fleet - 1
        }
    grow planet | isNeutral planet = planet
                | otherwise = addShips planet (planetGrowthRate planet)

stepAllFleets :: GameState -> GameState
stepAllFleets state | null (gameStateFleets state) = state
                    | otherwise = stepAllFleets $ step state

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
                -- Sort fleets, extract biggest two, calculate results
                let fleets' = sortBy (flip $ comparing snd) $
                        IM.assocs fleets
                    (bigWinner:bigLoser:_) = fleets'
                    remaining = snd bigWinner - snd bigLoser
                    planet' = planet { planetShips = remaining }
                -- Tie means ownership doesn't change.
                in if remaining > 0
                    then planet' { planetOwner = fst bigWinner }
                    else planet'

-- | Find the distance between two planets
--
turnsBetween :: Planet -> Planet -> Int
turnsBetween p1 = ceiling . distanceBetween p1

-- | Aux: Process order - create a new fleet, does nothing if order is impossible
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
                     dist = turnsBetween planetSrc planetDst
                     fleets'  = newFleet : (gameStateFleets state)
                 in GameState planets' fleets'

-- | Aux: Process a list of orders
--
processOrders :: [Order] -> GameState -> GameState
processOrders = flip $ foldr $ processOrder

-- | Aux: Process one tick of timer: planets are growing and fleets are moving
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

-- | Aux: Do all fights
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
modelStep :: [Order] -> GameState -> GameState
modelStep orders = fightAll . processTick . (processOrders orders)

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
    catch (loop mempty) $ \e -> if isEOFError e
        then return ()
        else ioError e
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

-- | Run a deterministic bot, dumping debug info
--
debugBot :: (GameState -> [Order])  -- ^ Deterministic AI function(
         -> IO ()                   -- ^ Blocks forever
debugBot f = do
    h <- openFile "debug.log" WriteMode 
    ioBot $ (run h)
    hClose h
  where
    run h s = do
        orders <- stateDump h s
        dumpIssue h orders
    dumpIssue log orders = do
        hPutStrLn log "\nOrders:"
        hPutStrLn log $ show orders
        hFlush log
        mapM_ issueOrder orders
    stateDump log s = do
        hPutStrLn log "\nState:"
        hPutStrLn log $ show s
        hFlush log
        return $ f s

-- | Read a game state from file. The format is the same as the server's output
-- for a turn. Useful when debugging.
--
stateFromFile :: FilePath     -- ^ Path to the file containing the game state.
              -> IO GameState -- ^ Parsed game state
stateFromFile path = withFile path ReadMode (read mempty)
  where
    read state handle = do
      line <- takeWhile (/= '#') <$> hGetLine handle
      if "go" `isPrefixOf` line
        then return state
        else read (buildGameState state line) handle

-- | Checks if a planet will survive the incoming fleets. A planet survives if
-- its owner is still the same after all known fleets arrive.
--
willSurviveAttack :: GameState -- ^ Initial game state
                  -> Int       -- ^ Planet ID
                  -> Bool      -- ^ Whether the planet survived
willSurviveAttack state pid = survives state
  where
    originalOwner = currentOwner state pid
    survives s = if null $ incomingFleets s pid
      then currentOwner s pid == originalOwner
      else survives $ step s

-- | The owner of a planet in a given game state.
--
currentOwner :: GameState -- ^ Current game state
             -> IM.Key    -- ^ Planet ID
             -> Int       -- ^ Owner ID
currentOwner state pid = owner $ gameStatePlanets state IM.! pid

-- | List of planets under attack, i.e., that have incoming fleets.
--
planetsUnderAttack :: GameState -- ^ Game state to analyze
                   -> [Int]     -- ^ List of IDs of planets under attack
planetsUnderAttack = (map fleetDestination) . gameStateFleets

-- | List of incoming fleets for a given planet in a certain game state.
--
incomingFleets :: GameState -- ^ Game state containing the current fleets
               -> Int       -- ^ Planet ID
               -> [Fleet]   -- ^ Incoming fleets
incomingFleets state pid = filter pidMatches fleets
  where
    pidMatches = (== pid) . fleetDestination
    fleets = gameStateFleets state

-- | Removes duplicates from a list of Ints
--
unique :: [Int] -> [Int]
unique = IS.toList . IS.fromList

