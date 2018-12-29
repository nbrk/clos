module Bresenham (bresenham) where

import           Data.List (sort, unfoldr)
import           Linear

type Point' = (Int,Int)

bresenham :: V2 Int -> V2 Int -> [V2 Int]
bresenham (V2 sx sy) (V2 tx ty) =
  let pts = bresenham' (sx, sy) (tx, ty)
  in
    map (uncurry V2) pts


bresenham' :: Point' -> Point' -> [Point']
bresenham' pa@(xa,ya) pb@(xb,yb) = map maySwitch . unfoldr go $ (x1,y1,0)
  where
    steep = abs (yb - ya) > abs (xb - xa)
    maySwitch = if steep then (\(x,y) -> (y,x)) else id
    [(x1,y1),(x2,y2)] = sort [maySwitch pa, maySwitch pb]
    deltax = x2 - x1
    deltay = abs (y2 - y1)
    ystep = if y1 < y2 then 1 else -1
    go (xTemp, yTemp, error)
        | xTemp > x2 = Nothing
        | otherwise  = Just ((xTemp, yTemp), (xTemp + 1, newY, newError))
        where
          tempError = error + deltay
          (newY, newError) = if (2*tempError) >= deltax
                            then (yTemp+ystep,tempError-deltax)
                            else (yTemp,tempError)
