module Main where

import System.Random
import Graphics.Gloss

main :: IO ()
main = do
    putStrLn "Input bound x (100 - 500): "
    boundXStr <- getLine
    let boundX = clamp 100 500 (read boundXStr :: Float)

    putStrLn "Input bound y (100 - 500): "
    boundYStr <- getLine
    let boundY = clamp 100 500 (read boundYStr :: Float)

    putStrLn "Input ball size (10 - 50): "
    ballRadiusStr <- getLine
    let ballRadius = clamp 10 50 (read ballRadiusStr :: Float)

    putStrLn "Input initial ball speed (50 - 100): "
    initVelocityStr <- getLine
    let initVelocity = clamp 50 100 (read initVelocityStr :: Float)
    let ballInitVelocity = (initVelocity, initVelocity)

    putStrLn "Input velocity increment (0 - 100): "
    incVelocityStr <- getLine
    let incVelocity = clamp 0 100 (read incVelocityStr :: Float)
    let ballIncVelocity = (incVelocity, incVelocity)

    putStrLn "Input maximal velocity (50 - 1000): "
    maxVelocityStr <- getLine
    let maxVelocity = clamp 100 1000 (read maxVelocityStr :: Float)
    let ballMaxVelocity = (maxVelocity, maxVelocity)

    x <- getStdRandom (randomR (-boundX, boundX))
    y <- getStdRandom (randomR (-boundY, boundY))

    let bound = (boundX, boundY)
    let ball = ((x, y), ballRadius, ballInitVelocity, ballIncVelocity, ballMaxVelocity, bound)
    simulate (window "Bouncing Ball" bound) black 60 ball drawBall (const updateBall)

-- Ball type
--      Position
--      Radius
--      Velocity
--      Velocity increment
--      Max velocity
--      Bound
type Ball = (Point, Float, Vector, Vector, Vector, Point)

window :: String -> Point -> Display
window title (boundX, boundY) = InWindow title  (2 * floor boundX, 2 * floor boundY) (0, 0)

drawBall :: Ball -> Picture
drawBall ((x, y), radius, _, _, _, _) = translate x y $ color white $ circleSolid radius

updateBall :: Float -> Ball -> Ball
updateBall dt ((x, y), radius, (dx, dy), (incDx, incDy), (maxDx, maxDy), (boundX, boundY)) = ((x', y'), radius, (dx', dy'), (incDx, incDy), (maxDx, maxDy), (boundX, boundY))
    where
        (x', dx') = clip x (boundX - radius) dx incDx maxDx
        (y', dy') = clip y (boundY - radius) dy incDy maxDy

        -- clip to a bounding interval
        clip h max dh incDh maxDh
            | h' > max      = ( max, -absDh')
            | h' < -max     = (-max, absDh')
            | otherwise     = (h', dh)
            where
                h'      = h + dt * dh
                absDh'  = min (abs dh + incDh) maxDh

clamp :: (Ord a)
      => a
      -- ^ The minimum value
      -> a
      -- ^ The maximum value
      -> a
      -- ^ The value to clamp
      -> a
clamp mn mx val = max mn (min val mx)

