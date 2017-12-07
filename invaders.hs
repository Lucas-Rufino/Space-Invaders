module Invaders(
    createInvader,
    createInvaderBullet,
    shootInvaders
) where

import Graphics.Rendering.OpenGL (GLdouble)
import Graphics.UI.Fungen
import Data.Foldable
import Textures
import Screen
import Types

createInvader :: GLdouble -> SIObject
createInvader x = do
  let sprite = Tex textureInvaderSize textureInvaderIndex
  object "invader" sprite False ((x * 64), heightGL - 25) (8.0, (-0.2)) ()

createInvaderBullet :: GLdouble -> GLdouble -> SIObject
createInvaderBullet x y = do
  let sprite = Tex textureInvaderBulletSize textureInvaderBulletIndex
  object "invaderBullet" sprite False (x, y) (0, -10) ()

shootInvaders :: SIAction ()
shootInvaders = do
  invaders <- getObjectsFromGroup "invadersGroup"
  forM_ invaders $ \invader -> do
    sleeping <- getObjectAsleep (invader)
    when (not sleeping) $ do
      (Score n) <- getGameAttribute
      let aux = ((fromIntegral n :: Double)/100.0)
      shoot <- randomDouble (0.0,100.0 + (aux))
      let boolean = (\x -> if x >= 99.0 then True else False)
      when (boolean (shoot)) $ do
         (pX, pY) <- getObjectPosition invader
         let obj = (createInvaderBullet (pX) (pY))
         addObjectsToGroup [obj] "invadersBulletGroup"
         drawObject obj