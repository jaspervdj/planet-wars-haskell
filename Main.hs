module Main where

import Data.List (isPrefixOf, intercalate, maximumBy, minimumBy)
import Control.Applicative ((<$>))
import Data.Monoid (Monoid, mempty, mappend)
import System.IO
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Ord (comparing)

data Planet = Planet
    { planetId         :: Int
    , planetOwner      :: Int
    , planetShips      :: Int
    , planetGrowthRate :: Int
    , planetX          :: Double
    , planetY          :: Double
    } deriving (Show)

data Fleet = Fleet
    { fleetOwner          :: Int
    , fleetShips          :: Int
    , fleetSource         :: Int
    , fleetDestination    :: Int
    , fleetTripLength     :: Int
    , fleetTurnsRemaining :: Int
    } deriving (Show)

data GameState = GameState
    { gameStatePlanets :: IntMap Planet
    , gameStateFleets  :: [Fleet]
    } deriving (Show)

instance Monoid GameState where
    mempty = GameState mempty mempty
    mappend (GameState p1 f1) (GameState p2 f2) =
        GameState (p1 `mappend` p2) (f1 `mappend` f2)

buildGameState :: GameState -> String -> GameState
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

issueOrder :: Handle  -- ^ Handle to write to
           -> Int     -- ^ Source planet
           -> Int     -- ^ Destination planet
           -> Int     -- ^ Number of ships
           -> IO ()   -- ^ Result
issueOrder handle source destination number = do
    hPutStrLn handle $ intercalate " " $ map show [source, destination, number]
    hFlush handle

finnishTurn :: Handle  -- ^ Handle to write to
            -> IO ()   -- ^ Result
finnishTurn handle = do
    hPutStrLn handle "go"
    hFlush handle

doTurn :: Handle     -- ^ Handle to write to
       -> GameState  -- ^ Game state
       -> IO ()
doTurn handle state = do
    if (null $ gameStateFleets state)
        -- Simple ai
        then issueOrder handle (planetId strongest) (planetId weakest) ships
        -- If we have a fleet in flight, just do nothing
        else return ()
  where
    strongest = maximumBy (comparing planetShips)
              $ filter ((== 1) . planetOwner)
              $ map snd $ IM.toList $ gameStatePlanets state
    weakest = minimumBy (comparing planetShips)
              $ filter ((/= 1) . planetOwner)
              $ map snd $ IM.toList $ gameStatePlanets state
    ships = planetShips strongest `div` 2

runGame :: Handle -> GameState -> IO ()
runGame handle state = do
    line <- takeWhile (/= '#') <$> hGetLine handle
    if "go" `isPrefixOf` line
        -- Go Go Go!
        then do
            doTurn stdout state
            finnishTurn stdout
            runGame handle mempty
        -- Keep building map
        else runGame handle (buildGameState state line)
    return ()

main :: IO ()
main = do
    let handle = stdin
    hSetBuffering stdin NoBuffering
    runGame stdin mempty
