module Textures(
    bmpList,
    textureBackgroundSize, textureBackgroundIndex,
    texturePlayerSize, texturePlayerIndex,
    textureInvaderSize, textureInvaderIndex,
    texturePlayerBulletSize, texturePlayerBulletIndex,
    textureInvaderBulletSize, textureInvaderBulletIndex,
    texturePowerUpSize, texturePowerUpIndex
) where

import Graphics.UI.Fungen

bmpList = [("sprites/background.bmp", Nothing),
           ("sprites/player.bmp", magenta),
           ("sprites/invader.bmp", magenta), 
           ("sprites/playerBullet.bmp", black),
           ("sprites/invaderBullet.bmp", black),
           ("sprites/powerUp.bmp", magenta)]

textureBackgroundIndex :: Int
textureBackgroundIndex = 0

texturePlayerIndex :: Int
texturePlayerIndex = 1

textureInvaderIndex :: Int
textureInvaderIndex = 2

texturePlayerBulletIndex :: Int
texturePlayerBulletIndex = 3

textureInvaderBulletIndex :: Int
textureInvaderBulletIndex = 4

texturePowerUpIndex :: Int
texturePowerUpIndex = 5

textureBackgroundSize :: (Double, Double)
textureBackgroundSize = (345, 600)

texturePlayerSize :: (Double, Double)
texturePlayerSize = (50, 50)

textureInvaderSize :: (Double, Double)
textureInvaderSize = (50, 50)

texturePlayerBulletSize :: (Double, Double)
texturePlayerBulletSize = (3, 6)

textureInvaderBulletSize :: (Double, Double)
textureInvaderBulletSize = (3, 6)

texturePowerUpSize :: (Double, Double)
texturePowerUpSize = (25, 25)

magenta :: InvList
magenta = Just [(255, 0, 255)]

black :: InvList
black = Just [(0,0,0)]