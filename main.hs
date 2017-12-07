module Main where

import Graphics.Rendering.OpenGL (GLdouble)
import Graphics.UI.Fungen
import Data.Foldable
import Textures
import Invaders
import Player
import Screen
import Types

main :: IO ()
main = do
  let winConfig = ((100,0), (width, height), "Space Invaders")
      gameMap = textureMap textureBackgroundIndex (fst textureBackgroundSize) (snd textureBackgroundSize) widthGL heightGL
      player = objectGroup "playerGroup" [createPlayer]
      playerBullet = objectGroup "playerBulletGroup" []
      invaders = objectGroup "invadersGroup" [createInvader (fromIntegral (i) :: Double) | i <- [1..5]]
      invadersBullet = objectGroup "invadersBulletGroup" []
      scoring = Score 0
      input = [
        (SpecialKey KeyRight, StillDown, moveRightPlayer),
        (SpecialKey KeyLeft, StillDown, moveLeftPlayer),
        (SpecialKey KeyDown, StillDown, moveDownPlayer),
        (SpecialKey KeyUp, StillDown, moveUpPlayer),
        (Char ' ', Press, shootPlayer),
        (Char 'q', Press, \_ _ -> funExit)]
  funInit winConfig gameMap [player, invaders, playerBullet, invadersBullet] () scoring input gameCycle (Timer 40) bmpList

gameCycle :: SIAction ()
gameCycle = do
  (Score n) <- getGameAttribute
  printOnScreen ("Score: " ++ show n) TimesRoman24 (10, 10) 1.0 1.0 1.0
  printOnScreen ("Level: " ++ show (n `div` 100)) TimesRoman24 (widthGL-100, 10) 1.0 1.0 1.0

  when ((n`mod`100)==0 && n>0) $ do
    invaders <- getObjectsFromGroup "invadersGroup"
    destroyObjects invaders
    addObjectsToGroup [createInvader (fromIntegral (i) :: Double) | i <- [1..5]] "invadersGroup"
    objs <- getObjectsFromGroup "invadersGroup"
    forM_ objs $ \invader -> do
      let aux = (fromIntegral n :: Double)/100.0
      setObjectSpeed (8.0,-0.2-(aux/2.0)) invader
    drawAllObjects
    
  invaders <- getObjectsFromGroup "invadersGroup"
  spaceShipBullets <- getObjectsFromGroup "playerBulletGroup"
  invaderBullets <- getObjectsFromGroup "invadersBulletGroup"
  player <- findObject "player" "playerGroup"

  shootInvaders
  forM_ invaders $ \invader -> do
    wallHit1 <- objectLeftMapCollision invader
    wallHit2 <- objectRightMapCollision invader
    when (wallHit1 || wallHit2) (reverseXSpeed invader)
    invassionSuccess <- objectBottomMapCollision invader
    crash <- objectsCollision invader player
    when (invassionSuccess || crash) $ funExit
    forM_ invaderBullets $ \b -> do
      spaceShipHit <- objectsCollision player b
      when spaceShipHit $ funExit
    forM_ spaceShipBullets $ \b -> do
      invaderHit <- objectsCollision invader b
      when invaderHit $ do
        setObjectAsleep True invader
        setObjectAsleep True b
        setGameAttribute (Score (n+20))
  forM_ invaders $ \invader1 -> do
    forM_ invaders $ \invader2 -> do
      invadersCrash <- objectsCollision invader1 invader2
      when invadersCrash $ do
        (reverseXSpeed invader1)
        (reverseXSpeed invader2)
