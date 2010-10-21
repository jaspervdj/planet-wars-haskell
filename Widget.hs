{-# Language TupleSections #-}
module Widget where

import Data.Maybe
import Data.List
import Control.Concurrent
import Control.Monad
import Graphics.Rendering.Cairo

import Graphics

type Pos = (Double,Double)

type Command = String

--data Mouse = Mouse | MousePressed Int
type Mouse = Maybe Int

data Menu = Menu [Widget] (MVar [(PShape,Int)]) (MVar Mouse)

newMenu :: [Widget] -> IO Menu
newMenu widgets = do
  svar <- newMVar []
  mvar <- newMVar Nothing
  return $ Menu widgets svar mvar

-- The pickable shape of a widget.
-- The computation of the size of a cairo text is done in the Render monad.
-- It is done during the rendering and kept for later (pure) reuse.
data PShape = PRectangle Double Double Double Double

pressMenu :: Pos -> Menu -> IO ()
pressMenu pos (Menu _ svar mvar) = do
  shapes <- readMVar svar
  _ <- takeMVar mvar
  putMVar mvar $ pickMenu pos shapes

releaseMenu :: Pos -> Menu -> IO (Maybe Int)
releaseMenu pos (Menu _ svar mvar) = do
  shapes <- readMVar svar
  v <- takeMVar mvar
  putMVar mvar Nothing
  case (pickMenu pos shapes, v) of
    (Just i, Just j) | i == j -> return $ Just i
    _ -> return Nothing

pickMenu :: Pos -> [(PShape,Int)] -> Maybe Int
pickMenu pos = fmap snd . find (pickPShape pos . fst)

pickPShape :: (Double, Double) -> PShape -> Bool
pickPShape (a,b) (PRectangle x y w h) = containXYWH a b x y w h

renderMenu :: Pos -> Menu -> Render ()
renderMenu pos (Menu widgets svar _) = do
  _ <- liftIO $ takeMVar svar
  pshapes <- renderMenu' pos widgets
  liftIO $ putMVar svar pshapes

renderMenu' :: Pos -> [Widget] -> Render [(PShape,Int)]
renderMenu' mouse menu = do
  ms <- zipWithM f menu [0..]
  return $ mapMaybe id ms
  where f w i = do
          s <- renderWidget mouse w
          return $ fmap (,i) s

data Widget =
    Label Pos String
  | Button Pos String Command
  deriving Show

sizeWidget :: Widget -> Render Double
sizeWidget (Label _ ss) = textWidth 18 20 ss
sizeWidget (Button _ ss _) = textWidth 18 20 ss

containWidget' :: Double -> (Double, Double) -> Widget -> Bool
containWidget' width (mx,my) (Label (x,y) _) =
  containXYWH mx my (x-10) (y-16) (width+21) 22
containWidget' width (mx,my) (Button (x,y) _ _) =
  containXYWH mx my (x-10) (y-16) (width+21) 22

renderWidget :: Pos -> Widget -> Render (Maybe PShape)
renderWidget _ (Label (x,y) ss) = do
  renderText x y black 18 20 ss
  return Nothing

renderWidget (mx,my) w@(Button (x,y) ss _) = do
  renderText x y black 18 20 ss

  width <- sizeWidget w
  when (containWidget' width (mx,my) w) $ do
    setLineWidth 2
    setSourceRGBA' black
    rectangle (x-10) (y-16) (width+21) 22
    stroke
  return . Just $ PRectangle (x-10) (y-16) (width+21) 22

textWidth :: Double -> Int -> String -> Render Double
textWidth sz n ss = do
  setFontSize sz
  let txts = chop n ss
      f s = textExtentsWidth `fmap` textExtents s
  widths <- mapM f txts
  return $ maximum widths

renderText :: Double -> Double -> RGBA -> Double -> Int -> String -> Render ()
renderText x y rgba sz n ss = do
  setFontSize sz
  setSourceRGBA' rgba
  let txts = zip [(x,y + yi) | yi <- [0,sz*1.2..]] $ chop n ss
      f ((xi,yi),s) = do
        moveTo xi yi
        showText s
  mapM_ f txts

-- Givent a string, break it into a list of strings
-- - at each '\n'
-- - so that the sum of the words of each string is <= n
chop :: Int -> String -> [String]
chop n str = concatMap (f n [] [] . words) ls
  where ls = lines str
        f _ xs ys [] = ys ++ [unwords xs]
        f n' xs ys (w:ws) = let m = max n' (length w) in
          if sum (map length xs) + length w > m
          then f m [] (ys ++ [unwords xs]) (w:ws)
          else f m (xs++[w]) ys ws
