module PlanetWarsGraphics where

import Graphics.Rendering.Cairo

import PlanetWars
import Graphics

renderPlanetWars :: (Double,Double) -> GameState -> Render ()
renderPlanetWars pos game = do
  mapM_ (renderPlanet pos) (planets game)
  mapM_ (renderFleet game) (gameStateFleets game)

renderPlanet :: (Double,Double) -> Planet -> Render ()
renderPlanet (x,y) p = do
  let xc = planetX p * 15 + 60 -- local coord. changes
      yc = planetY p * 15 + 60
      radius = (fromIntegral (planetGrowthRate p) * 3) + 10
      color = case planetOwner p of
        0 -> grey
        1 -> blue  -- us
        2 -> red
        _ -> green -- more than two players
  newPath
  setSourceRGBA' color
  setFontSize 10

  arc xc yc radius 0 (2*pi)
  if sq (x - xc) + sq (y - yc) < sq (radius + 3)
    then setLineWidth 3
    else setLineWidth 1
  stroke

  moveTo xc yc
  showText (show $ planetShips p)

  moveTo xc (yc + 10)
  showText $ "+" ++ (show $ planetGrowthRate p)

renderFleet :: GameState -> Fleet -> Render ()
renderFleet g f = do
  let src = getPlanetById (fleetSource f) g
      dest = getPlanetById (fleetDestination f) g
      xs = planetX src * 15 + 60 -- local coord. changes
      ys = planetY src * 15 + 60
      xd = planetX dest * 15 + 60
      yd = planetY dest * 15 + 60
      xc = xd + (xs - xd) * fromIntegral (fleetTurnsRemaining f) / fromIntegral (fleetTripLength f)
      yc = yd + (ys - yd) * fromIntegral (fleetTurnsRemaining f) / fromIntegral (fleetTripLength f)
      radius = 3
      color = case fleetOwner f of
        0 -> grey  -- won't happen
        1 -> blue  -- us
        2 -> red
        _ -> green -- more than two players
  newPath
  setSourceRGBA' color
  setFontSize 10
  setLineWidth 0.6

  arc xc yc radius 0 (2*pi)
  fill

  moveTo xs ys
  setDash [] 0
  lineTo xc yc
  stroke
  moveTo xc yc
  setDash [4,4] 0
  lineTo xd yd
  stroke

  moveTo (xc-10) (yc-8)
  showText (show $ fleetShips f)

data Interaction =
    None
  | SourceSelected Int
  | DestinationSelected Int Int

renderInteraction :: (Double, Double) -> Interaction -> GameState -> Render ()
renderInteraction (x,y) i g = case i of
  None -> return ()
  SourceSelected s -> do
    setSourceRGBA' black
    let src = getPlanetById s g
        xs = planetX src * 15 + 60 -- local coord. changes
        ys = planetY src * 15 + 60
    setSourceRGBA' black
    setLineWidth 2
    moveTo xs ys
    lineTo x y
    stroke
  DestinationSelected s d -> do
    setSourceRGBA' black
    let src = getPlanetById s g
        dest = getPlanetById d g
        xs = planetX src * 15 + 60 -- local coord. changes
        ys = planetY src * 15 + 60
        xd = planetX dest * 15 + 60
        yd = planetY dest * 15 + 60
    setSourceRGBA' black
    setLineWidth 2
    moveTo xs ys
    lineTo xd yd
    stroke

    newPath
    setSourceRGBA' grey
    setDash [4,4] 0
    let radius = sqrt $ sq (x-xs) + sq (y-ys)
    arc xs ys radius 0 (2*pi)
    stroke

    moveTo x (y-10)
    let l = max (1::Int) (ceiling (radius / 2) - 10)
    setFontSize 14
    showText (show l)

renderOrder :: GameState -> Order -> Render ()
renderOrder g (Order s d a) = do
  setSourceRGBA' black
  let src = getPlanetById s g
      dest = getPlanetById d g
      xs = planetX src * 15 + 60 -- local coord. changes
      ys = planetY src * 15 + 60
      xd = planetX dest * 15 + 60
      yd = planetY dest * 15 + 60
  setSourceRGBA' black
  setLineWidth 2
  moveTo xs ys
  lineTo xd yd
  stroke

click :: Interaction -> (Double,Double) -> GameState -> Either Interaction Order
click int pos game = case int of
    None -> case pickPlanetWars pos game of
      Nothing -> Left int
      Just s -> Left $ SourceSelected s
    SourceSelected s -> case pickPlanetWars pos game of
      Nothing -> Left int
      Just d -> Left $ DestinationSelected s d
    DestinationSelected s d ->
      let a = pickAmount pos game s
      in Right $ Order s d a

pickPlanetWars :: (Double, Double) -> GameState -> Maybe Int
pickPlanetWars pos game =
  case filter (pickPlanet pos) $ planets game of
    [] -> Nothing
    (x:_) -> Just $ planetId x

pickPlanet :: (Double,Double) -> Planet -> Bool
pickPlanet (x,y) p =
  let xc = planetX p * 15 + 60 -- local coord. changes
      yc = planetY p * 15 + 60
      radius = (fromIntegral (planetGrowthRate p) * 3) + 10
  in if sq (x - xc) + sq (y - yc) < sq (radius + 3)
     then True
     else False

pickAmount :: (Double,Double) -> GameState -> Int -> Int
pickAmount (x,y) g s =
  let src = getPlanetById s g
      xs = planetX src * 15 + 60 -- local coord. changes
      ys = planetY src * 15 + 60
      radius = sqrt $ sq (x-xs) + sq (y-ys)
  in max 1 (ceiling (radius / 2) - 10)

sq :: Double -> Double
sq x = x * x
