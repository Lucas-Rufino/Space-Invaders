module Player(
    createPlayer,
    createPlayerBullet,
    moveRightPlayer,
    moveLeftPlayer,
    moveUpPlayer,
    moveDownPlayer,
    shootPlayer
) where

import Graphics.Rendering.OpenGL (GLdouble)
import Graphics.UI.Fungen
import Textures
import Screen
import Types

createPlayer :: SIObject
createPlayer =
  let sprite = Tex texturePlayerSize texturePlayerIndex
  in object "player" sprite False (widthGL/2, 100) (0, 0) ()

createPlayerBullet :: GLdouble -> GLdouble -> SIObject
createPlayerBullet x y =
  let sprite = Tex texturePlayerBulletSize texturePlayerBulletIndex
  in object "playerBullet" sprite False (x, y) (0, 30) ()

moveRightPlayer :: Modifiers -> Position -> SIAction ()
moveRightPlayer _ _ = do
  obj <- findObject "player" "playerGroup"
  (px, py) <- getObjectPosition obj
  (sx, _ ) <- getObjectSize obj
  if(px + (sx/2) + 7 <= widthGL)
    then (setObjectPosition ((px + 7), py) obj)
    else (setObjectPosition ((widthGL - (sx/2)), py) obj)

moveLeftPlayer :: Modifiers -> Position -> SIAction ()
moveLeftPlayer _ _ = do
  obj <- findObject "player" "playerGroup"
  (px, py) <- getObjectPosition obj
  (sx, _ ) <- getObjectSize obj
  if(px - (sx/2) - 7 >= 0)
    then (setObjectPosition ((px - 7), py) obj)
    else (setObjectPosition (sx/2, py) obj)

moveUpPlayer :: Modifiers -> Position -> SIAction ()
moveUpPlayer _ _ = do
    obj <- findObject "player" "playerGroup"
    (px, py) <- getObjectPosition obj
    ( _, sy) <- getObjectSize obj
    if(py + (sy/2) < heightGL)
        then (setObjectPosition (px, (py + 7)) obj)
        else (setObjectPosition (px, heightGL - (sy/2)) obj)

moveDownPlayer :: Modifiers -> Position -> SIAction ()
moveDownPlayer _ _ = do
    obj <- findObject "player" "playerGroup"
    (px, py) <- getObjectPosition obj
    ( _, sy) <- getObjectSize obj
    if(py - (sy/2) - 7 > 0)
        then (setObjectPosition (px, (py - 7)) obj)
        else (setObjectPosition (px, (sy/2)) obj)

shootPlayer :: Modifiers -> Position -> SIAction ()
shootPlayer _ _ = do
  obj <- findObject "player" "playerGroup"
  (px, py) <- getObjectPosition obj
  let obj = (createPlayerBullet (px) (py))
  addObjectsToGroup [obj] "playerBulletGroup"
  drawObject obj
