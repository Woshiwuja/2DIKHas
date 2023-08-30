module Main where

data Point = Point {
    x :: Float,
    y :: Float
} deriving (Show)

-- Funzione per convertire gradi in radianti
radians x = x*(pi/180)

--Funzione per segare cifre decimali, cambiare 10^n dove n numero decimali
roundF :: Float -> Float
roundF x = fromIntegral(floor(x * t))/t
    where t = 10^2

-- Rotazione x' = x cos(a) - y sin a e seguente

rotateX :: Float -> Point -> Float
rotateX alpha (Point x y) = x * cos (radians alpha) - y * sin (radians alpha)

rotateY :: Float -> Point -> Float
rotateY alpha (Point x y) = x * sin (radians alpha) + y * cos (radians alpha)

pointDistance :: Point -> Point -> Float
pointDistance (Point x y) (Point x1 y1) = sqrt((x1 - x)^2+(y1-y)^2)
-- distanza fra 2 punti date le coordinate

main = do
    let p0 = Point 0 1
    let alpha = 90
    let finalX = roundF(rotateX alpha p0)
    let finalY = roundF(rotateY alpha p0)
    let p1 = Point finalX finalY
    let distance = pointDistance p0 p1  
    print p1
    print distance