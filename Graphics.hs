module Graphics where

import Graphics.Rendering.Cairo

----------------------------------------------------------------------
-- Data
----------------------------------------------------------------------

type RGBA = (Double,Double,Double,Double)

black :: RGBA
black = (0,0,0,1)

white :: RGBA
white = (1,1,1,1)

red :: RGBA
red = (1,0,0,1)

green :: RGBA
green = (0,1,0,1)

blue :: RGBA
blue = (0,0,1,1)

grey :: RGBA
grey = (0.6,0.6,0.6,1)

lightGrey :: RGBA
lightGrey = (0.9,0.9,0.9,1)

cyan :: RGBA
cyan = (0,1,1,1)

magenta :: RGBA
magenta = (1,0,1,1)

orange :: RGBA
orange = (1,0.7,0,1)

setSourceRGBA' :: RGBA -> Render ()
setSourceRGBA' (r,g,b,a) = setSourceRGBA r g b a

-- Test if the rectangle (x,y,w,h) contains the point (a,b).
containXYWH :: Double -> Double -> Double -> Double -> Double -> Double -> Bool
containXYWH a b x y w h =
  a >= x && b >= y && a <= x + w && b <= y + h

