module Shapes
    ( Point(..)
    , Shape(..)
    , area
    , nudge
    , baseCircle
    , baseRectangle
    ) where

data NewBool = True | False

data Point = Point Float Float deriving (Show)

data Shape = Circle Point Float | Rectangle Point Point
    deriving (Show)

-- Pattern match against constructors
area :: Shape -> Float
area (Circle _ r) = pi * r ^ 2
area (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

areaOfARectangle = area $ Rectangle (Point 0 0) (Point 100 100)
areaOfACircle = area $ Circle (Point 0 0 ) 24

-- You can partially apply constructors, just like any other function
partiallyAppliedCircle = Circle $ Point 10 10
mappedCircles = map partiallyAppliedCircle [4,5,4,1]

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x + a) (y + b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1 + a) (y1 + a)) (Point (x2 + b) (y2 + b))

nudgedRectangle = nudge (Rectangle (Point 0 0) (Point 10 10)) 1 3

baseCircle :: Float -> Shape
baseCircle = Circle (Point 0 0)

baseRectangle :: Float -> Float -> Shape
baseRectangle width height = Rectangle (Point 0 0) (Point width height)

createFromBaseRectangle :: Float -> Float -> Float -> Float -> Shape
createFromBaseRectangle width height = nudge $ baseRectangle width height