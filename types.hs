module Types(
    GameAttribute(Score),
    GameState(Level),
    SIObject,
    SIAction,
    convertGL
) where

import Graphics.Rendering.OpenGL (GLdouble)
import Graphics.UI.Fungen

convertGL :: Double -> GLdouble
convertGL value = value :: GLdouble

data GameAttribute = Score Int
data GameState = Level Int

type SIObject = GameObject ()
type SIAction a = IOGame GameAttribute () () () a