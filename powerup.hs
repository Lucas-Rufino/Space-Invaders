module PowerUp(
    createPowerUp
) where

import Graphics.Rendering.OpenGL (GLdouble)
import Graphics.UI.Fungen
import Textures
import Types

createPowerUp :: GLdouble -> GLdouble -> SIObject
createPowerUp x y =
    let sprite = Tex texturePowerUpSize texturePowerUpIndex
    in object "powerUp" sprite False (x, y) (0, 0) ()