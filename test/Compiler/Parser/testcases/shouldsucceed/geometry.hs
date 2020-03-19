module Geometry where

data Point = Point Float Float
data Shape = Circle Point Float | Rectangle Point Float Float

area :: Shape -> Float
area (Circle _ r) = pi * r ^ 2

sphereVolume :: Float -> Float
sphereVolume r = (4.0/3.0) * pi * (r ^ 3)

sphereArea :: Float -> Float
sphereArea r = 4 * pi * (r ^ 2)

cubeVolume :: Float -> Float
cubeVolume s = cuboidVolume s s s

cubeArea :: Float -> Float
cubeArea s = cuboidArea s s s

cuboidVolume :: Float -> Float -> Float -> Float
cuboidVolume a b c = a * b * c

cuboidArea :: Float -> Float -> Float -> Float
cuboidArea a b c = 2 * (a * b + b * c + a * c)
