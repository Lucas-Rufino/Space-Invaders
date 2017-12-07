module Screen(
    width,
    height,
    screenSize,
    widthGL,
    heightGL,
    middle
) where

import Graphics.Rendering.OpenGL (GLdouble)
import Graphics.UI.Fungen

width :: Int
width = 345

height :: Int
height = 600

screenSize :: (Int, Int)
screenSize = (width, height)

widthGL :: GLdouble
widthGL = fromIntegral width :: GLdouble

heightGL :: GLdouble
heightGL = fromIntegral height :: GLdouble

middle :: (Double, Double)
middle = (fromIntegral (width `div` 2), fromIntegral (height `div` 2))
