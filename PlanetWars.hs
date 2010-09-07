module PlanetWars
    ( Planet (..)
    , Fleet (..)
    , Order (..)
    , GameState (..)
    , isMyPlanet
    , addShips
    , bot
    ) where

import Control.Applicative ((<$>))
import Data.List (intercalate, isPrefixOf)
import Data.Monoid (Monoid, mempty, mappend)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import System.IO

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

data Order = Order
    { orderSource      :: Int
    , orderDestination :: Int
    , orderShips       :: Int
    } deriving (Show)

data GameState = GameState
    { gameStatePlanets :: IntMap Planet
    , gameStateFleets  :: [Fleet]
    } deriving (Show)

instance Monoid GameState where
    mempty = GameState mempty mempty
    mappend (GameState p1 f1) (GameState p2 f2) =
        GameState (p1 `mappend` p2) (f1 `mappend` f2)

issueOrder :: Order -> IO ()
issueOrder (Order source destination ships) =
    putStrLn $ intercalate " " $ map show [source, destination, ships]

finnishTurn :: IO ()   -- ^ Result
finnishTurn = do
    putStrLn "go"
    hFlush stdout

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

isMyPlanet :: Planet -> Bool
isMyPlanet = (== 1) . planetOwner

addShips :: Planet -> Int -> Planet
addShips planet n = planet {planetShips = planetShips planet + n}

bot :: (GameState -> [Order])
    -> IO ()
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
