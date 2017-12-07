module Types(
    GameAttribute(Score),
    GameState(Level),
    SIObject,
    SIAction
) where

import Graphics.UI.Fungen

data GameAttribute = Score Int
data GameState = Level Int

type SIObject = GameObject ()
type SIAction a = IOGame GameAttribute () () () a