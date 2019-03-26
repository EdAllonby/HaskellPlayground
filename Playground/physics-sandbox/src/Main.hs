{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Lens
import           Graphics.Gloss
import           Graphics.Gloss.Data.Color

window = InWindow "Sandbox" (1500, 600) (100, 100)

fps = 60

data Box =
  Box { _xPos :: Float, _mass :: Float, _size :: Float, _velocity :: Float }
  deriving (Eq, Show)

data Sandbox =
  Sandbox { _leftBox :: Box, _rightBox :: Box, _collisions :: Int }
  deriving (Eq, Show)

makeLenses ''Box

makeLenses ''Sandbox

initialSandbox :: Sandbox
initialSandbox = Sandbox (Box 100 1 20 0) (Box 200 100 50 (-2)) 0

backgroundColor :: Color
backgroundColor = white

main :: IO ()
main = play window backgroundColor fps initialSandbox draw (const id) frame

frame :: Float -> Sandbox -> Sandbox
frame delta sandbox =
  if collided (sandbox ^. leftBox) (sandbox ^. rightBox)
  then (updateCount . moveLeft . moveRight . updateVelocitiesAfterBounce)
    sandbox
  else (moveLeft . moveRight) sandbox
  where
    moveLeft = updateBoxPosition delta leftBox

    moveRight = updateBoxPosition delta rightBox

    bounceLeft = bounce leftBox rightBox

    bounceRight = bounce rightBox leftBox

updateCount :: Sandbox -> Sandbox
updateCount sandbox = sandbox & collisions %~ (+ 1)

updateBoxPosition :: Float -> Lens' Sandbox Box -> Sandbox -> Sandbox
updateBoxPosition delta boxLens sandbox =
  if hitWall $ sandbox ^. boxLens
  then updateCount
    $ getNextPosition delta boxLens
    $ over boxLens reverseBox sandbox
  else getNextPosition delta boxLens sandbox

getNextPosition :: Float -> Lens' Sandbox Box -> Sandbox -> Sandbox
getNextPosition delta boxLens sandbox = over
  (boxLens . xPos)
  (+ (delta * fromIntegral fps * (sandbox ^. boxLens . velocity)))
  sandbox

updateVelocitiesAfterBounce :: Sandbox -> Sandbox
updateVelocitiesAfterBounce sandbox = sandbox
  & (leftBox . velocity) .~ newLeftVelocity
  & (rightBox . velocity) .~ newRightVelocity
  where
    newLeftVelocity = bounce leftBox rightBox sandbox

    newRightVelocity = bounce rightBox leftBox sandbox

bounce :: Lens' Sandbox Box -> Lens' Sandbox Box -> Sandbox -> Float
bounce targetBox otherBox sandbox = newV
  where
    targetMass = sandbox ^. (targetBox . mass)

    otherMass = sandbox ^. (otherBox . mass)

    targetVelocity = sandbox ^. (targetBox . velocity)

    otherVelocity = sandbox ^. (otherBox . velocity)

    sumM = targetMass + otherMass

    newV = (targetMass - otherMass) / sumM * targetVelocity
      + ((2 * otherMass / sumM) * otherVelocity)

collided :: Box -> Box -> Bool
collided left right = leftEdge >= rightEdge
  where
    leftEdge = (left ^. xPos) + (left ^. size / 2)

    rightEdge = (right ^. xPos) - (right ^. size / 2)

hitWall :: Box -> Bool
hitWall box = box ^. xPos - (box ^. size / 2) <= 0

reverseBox :: Box -> Box
reverseBox = over velocity negate

draw :: Sandbox -> Picture
draw sandbox = pictures
  [ color black (line [(0, -300), (-0, 300)])
  , translate (sandbox ^. rightBox . xPos) (sandbox ^. rightBox . size / 2)
      $ color red
      $ rectangleSolid
        (sandbox ^. rightBox . size)
        (sandbox ^. rightBox . size)
  , translate (sandbox ^. leftBox . xPos) (sandbox ^. leftBox . size / 2)
      $ color blue
      $ rectangleSolid (sandbox ^. leftBox . size) (sandbox ^. leftBox . size)
  , text $ show (sandbox ^. collisions)]
