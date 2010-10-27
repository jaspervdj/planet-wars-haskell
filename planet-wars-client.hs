module Main where

import System.Environment (getArgs)
import System.IO (hSetBuffering, stdin, BufferMode(NoBuffering))

import Graphics.UI.Gtk hiding (
  eventKeyName, eventButton, eventModifier, Menu, Rectangle, Widget)
import Graphics.UI.Gtk.Gdk.Events (
  eventX, eventY, eventKeyName, eventButton, eventDirection, eventModifier)
import Graphics.Rendering.Cairo

import Control.Concurrent

import Data.Monoid (mempty)
import Monad (foldM)
import PlanetWars
import PlanetWarsGraphics
import Graphics
import Widget

----------------------------------------------------------------------
-- The main program
----------------------------------------------------------------------

background :: RGBA
background = white

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      hSetBuffering stdin NoBuffering
      gs <- newGameState
      main' $ cleanState
        { widgets =
          [ Label (10,20) "Planet Wars"
          , Button (140,20) "next" "next"
          , Label (240,20) "turn #0"
          ]
        , game = gs
        }
    _ -> putStrLn "usage..."

main' :: S -> IO ()
main' initialState = do
  initGUI
  window <- windowNew
  set window
    [ windowTitle := "Planet Wars Client"
    , windowDefaultWidth := 320
    , windowDefaultHeight := 200
    , containerBorderWidth := 0
    ]
  canvas <- drawingAreaNew
  containerAdd window canvas
  widgetShowAll window 

  sVar <- newMVar initialState
  menu <- newMenu (widgets initialState)

  onKeyPress window $ \e -> do
    s <- takeMVar sVar
    ms <- myKeyPress (eventKeyName e) s
    case ms of
      Nothing -> do
        putMVar sVar s
        return ()
      Just s' -> do
        putMVar sVar s'
        widgetQueueDraw canvas
    return True

  onButtonPress canvas $ \e -> do
    case eventButton e of
      LeftButton -> do
        s <- takeMVar sVar
        s' <- myLmbPress (Control `elem` eventModifier e) (eventX e) (eventY e) s menu
        putMVar sVar s'
        widgetQueueDraw canvas
      _ -> return ()
    return True

  onButtonRelease canvas $ \e -> do
    case eventButton e of
      LeftButton -> do
        s <- takeMVar sVar
        s' <- myLmbRelease (eventX e) (eventY e) s menu
        putMVar sVar s'
        widgetQueueDraw canvas
      _ -> return ()
    return True

  onScroll canvas $ \e -> do
    case eventDirection e of
      ScrollUp -> do
        s <- takeMVar sVar
        putMVar sVar (myScroll True s)
        widgetQueueDraw canvas
      ScrollDown -> do
        s <- takeMVar sVar
        putMVar sVar (myScroll False s)
        widgetQueueDraw canvas
      _ -> return ()
    return True

  onMotionNotify canvas False $ \e -> do
    s <- takeMVar sVar
    -- The first time onMotionNotify is called, the computed dx
    -- and dy are wrong.
    let dx = eventX e - mouseX s
        dy = eventY e - mouseY s
    let lmb = Button1 `elem` (eventModifier e)
        rmb = Button3 `elem` (eventModifier e)
    s' <- myMotion lmb rmb dx dy $ s { mouseX = eventX e, mouseY = eventY e }
    putMVar sVar s'
    widgetQueueDraw canvas
    return True

  onExpose canvas $ \_ -> do
    (w,h) <- widgetGetSize canvas
    drawin <- widgetGetDrawWindow canvas
    s <- readMVar sVar
    renderWithDrawable drawin (myDraw
      (s { width = fromIntegral w, height = fromIntegral h })
      menu)
    return True
 
  onDestroy window mainQuit
  mainGUI

----------------------------------------------------------------------
-- The main state of the program
----------------------------------------------------------------------

data S = S
  { width :: Double
  , height :: Double
  , mouseX :: Double
  , mouseY :: Double
  , panX :: Double
  , panY :: Double
  , zoom :: Double
  , widgets :: [Widget]
  , game :: GameState
  , interaction :: Interaction
  , orders :: [Order]
  }

cleanState :: S
cleanState = S
  { width = 320
  , height = 200
  , mouseX = 0
  , mouseY = 0
  , panX = 0
  , panY = 0
  , zoom = 1
  , widgets = []
  , game = mempty
  , interaction = None
  , orders = []
  }

----------------------------------------------------------------------
-- The main callbacks
----------------------------------------------------------------------

myKeyPress :: String -> S -> IO (Maybe S)
myKeyPress k s = case k of
  "plus" -> return . Just $ zoomAt (mouseX s) (mouseY s) 1.1 s
  "minus" -> return . Just $ zoomAt (mouseX s) (mouseY s) (1 / 1.1) s
  "Up" -> return . Just $ pan 0 20 s
  "Down" -> return . Just $ pan 0 (-20) s
  "Left" -> return . Just $ pan 20 0 s
  "Right" -> return . Just $ pan (-20) 0 s
  "Escape" -> mainQuit >> return Nothing
  _ -> return Nothing

myLmbPress :: Bool -> Double -> Double -> S -> Menu -> IO S
myLmbPress ctrl x y s menu = do
  let (x',y') = screenToScene s (x,y)

  let (interaction', ords) =
        case click (interaction s) (x',y') (game s) of
          Left i -> (i, [])
          Right o -> (None, [o])

  pressMenu (x',y') menu

  return $ s
    { interaction = interaction'
    , orders = orders s ++ ords
    }

myLmbRelease :: Double -> Double -> S -> Menu -> IO S
myLmbRelease x y s menu@(Menu ws _ _) = do
  let (x',y') = screenToScene s (x,y)
  b <- releaseMenu (x',y') menu
  (o,gs') <- case b of
    Nothing -> return (orders s,game s)
    Just i -> case ws !! i of
      Button _ _ "next" -> do
        foldM  issueOrder' (game s) (orders s)
        finishTurn
        gs <- newGameState
        return ([],gs)
      _ -> mainQuit >> return undefined

  return $ s {orders = o, game = gs' }

-- The bool specifies if it is up (true) or down (false).
myScroll :: Bool -> S -> S
myScroll up s = if up
  then zoomAt (mouseX s) (mouseY s) 1.1 s
  else zoomAt (mouseX s) (mouseY s) (1 / 1.1) s

-- The bools specifies if the lmb and rmb are pressed.
myMotion :: Bool -> Bool -> Double -> Double -> S -> IO S
myMotion False True dx dy s = return $ pan dx dy s
myMotion _ _ _ _ s = return s

myDraw :: S -> Menu -> Render ()
myDraw s menu = do
  -- clear
  setSourceRGBA' background
  paint

  -- view the scene under the pan/zoom transform
  translate (panX s) (panY s)
  scale (zoom s) (zoom s)

  -- render widgets
  let pos = screenToScene s (mouseX s, mouseY s)
  renderMenu pos menu

  -- render planet wars
  renderPlanetWars pos (game s)
  renderInteraction pos (interaction s) (game s)
  mapM_ (renderOrder $ game s) (orders s)

----------------------------------------------------------------------
-- Convenience functions
----------------------------------------------------------------------

-- Transform from screen coordinate to scene coordinate.
screenToScene :: S -> (Double,Double) -> (Double,Double)
screenToScene s (x,y) = ((x - panX s) / zoom s, (y - panY s) / zoom s)

-- Transform from screen coordinate delta to scene coordinate delta.
screenToSceneDelta :: S -> (Double, Double) -> (Double, Double)
screenToSceneDelta s (dx,dy) = (dx / zoom s, dy / zoom s)

-- Add dx and dy to the pan.
pan :: Double -> Double -> S -> S
pan dx dy s = s { panX = panX s + dx, panY = panY s + dy }

-- Multiply the zoom by a, modifying the panX and panY values
-- so that the scene-point under the screen coordinate (x,y)
-- remains at the same screen coordiante.
zoomAt :: Double -> Double -> Double -> S -> S
zoomAt x y a s =
  let (x1,y1) = screenToScene s (x,y)
      s' = s { zoom = zoom s * a }
      (x2,y2) = screenToScene s' (x,y)
  in s'
    { panX = panX s' - (x1 - x2) * zoom s'
    , panY = panY s' - (y1 - y2) * zoom s'
    }

