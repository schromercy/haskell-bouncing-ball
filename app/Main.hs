module Main where

import System.Random
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Simulate
import Data.Time
import Text.Read

main :: IO ()
main = do
    writeLineLog "Bouncing ball initialized..."

    -- Initialize values.
    putStrLn "Input bound x (100 - 500): "
    boundXStr <- getLine
    let boundX = clamp 100 500 (read boundXStr :: Float)
    appendLineLog $ "Bound X is " ++ show boundX

    putStrLn "Input bound y (100 - 500): "
    boundYStr <- getLine
    let boundY = clamp 100 500 (read boundYStr :: Float)
    appendLineLog $ "Bound Y is " ++ show boundY

    putStrLn "Input ball size (10 - 50): "
    ballRadiusStr <- getLine
    let ballRadius = clamp 10 50 (read ballRadiusStr :: Float)
    appendLineLog $ "Ball radius is " ++ show ballRadius

    putStrLn "Input initial ball speed (50 - 100): "
    initVelocityStr <- getLine
    let initVelocity = clamp 50 100 (read initVelocityStr :: Float)
    let ballInitVelocity = (initVelocity, initVelocity)
    appendLineLog $ "Ball initial velocity is " ++ show initVelocity

    putStrLn "Input velocity increment (0 - 100): "
    incVelocityStr <- getLine
    let incVelocity = clamp 0 100 (read incVelocityStr :: Float)
    let ballIncVelocity = (incVelocity, incVelocity)
    appendLineLog $ "Ball incremental velocity is " ++ show incVelocity

    putStrLn "Input maximal velocity (50 - 1000): "
    maxVelocityStr <- getLine
    let maxVelocity = clamp 100 1000 (read maxVelocityStr :: Float)
    let ballMaxVelocity = (maxVelocity, maxVelocity)
    appendLineLog $ "Ball maximal velocity is " ++ show maxVelocity

    x <- getStdRandom (randomR (-boundX, boundX))
    y <- getStdRandom (randomR (-boundY, boundY))
    appendLineLog $ "Ball random position is (" ++ show x ++ ", " ++ show y ++ ")"

    let bound = (boundX, boundY)
    let ball = ((x, y), ballRadius, ballInitVelocity, ballIncVelocity, ballMaxVelocity, bound, [white, red, green, blue])

    -- Start simulation
    appendLineLog "Starting simulation..."
    simulateIO (window "Bouncing Ball" bound) black 60 ball drawBall (const updateBall)

-- Ball type
--      Position
--      Radius
--      Velocity
--      Velocity increment
--      Max velocity
--      Bound
--      Possible Colors
type Ball = (Point, Float, Vector, Vector, Vector, Point, [Color])

window :: String -> Point -> Display
window title (boundX, boundY) = InWindow title  (2 * floor boundX, 2 * floor boundY) (0, 0)

drawBall :: Ball -> IO Picture
drawBall ((x, y), radius, _, _, _, _, colors) = do
    return (translate x y $ color (head colors) $ circleSolid radius)

updateBall :: Float -> Ball -> IO Ball
updateBall dt ((x, y), radius, (dx, dy), (incDx, incDy), (maxDx, maxDy), (boundX, boundY), colors) = do
        (x', dx', isBounceX) <- clip "X" x (boundX - radius) dx incDx maxDx
        (y', dy', isBounceY) <- clip "Y" y (boundY - radius) dy incDy maxDy
        colors' <- updateColors (isBounceX || isBounceY)
        return ((x', y'), radius, (dx', dy'), (incDx, incDy), (maxDx, maxDy), (boundX, boundY), colors')
        where
            -- clip to a bounding interval
            clip axis h max dh incDh maxDh
                | h' > max  = do
                    appendLineLog $ "Bounce, " ++ axis ++ "-speed now: " ++ show absDh'
                    return ( max, -absDh', True)
                | h' < -max = do
                    appendLineLog $ "Bounce, " ++ axis ++ "-speed now: " ++ show absDh'
                    return (-max, absDh', True)
                | otherwise = do
                    return (h', dh, False)
                where
                    h'      = h + dt * dh
                    absDh'  = min (abs dh + incDh) maxDh
            
            -- update colors
            updateColors isUpdate
                | isUpdate = do
                    let newColors = popAndPushArray colors
                    appendLineLog $ "Ball new color is " ++ show (head newColors)
                    return newColors
                | otherwise = return colors

clamp :: (Ord a)
      => a
      -- ^ The minimum value
      -> a
      -- ^ The maximum value
      -> a
      -- ^ The value to clamp
      -> a
clamp mn mx val = max mn (min val mx)

logFileName :: String
logFileName = "log.txt"

writeLineLog :: String -> IO ()
writeLineLog message = do
    formattedMessage <- formatLogMessage message
    let finalMessage = formattedMessage ++ "\n"
    writeFile logFileName finalMessage

appendLineLog :: String -> IO ()
appendLineLog message = do
    formattedMessage <- formatLogMessage message
    let finalMessage = formattedMessage ++ "\n"
    appendFile logFileName finalMessage

formatLogMessage :: String -> IO String
formatLogMessage message = do
    zonedTime <- getZonedTime
    return $ show zonedTime ++ ": " ++ message

popAndPushArray :: [a] -> [a]
popAndPushArray (x:xs) = xs ++ [x]
popAndPushArray arr = arr
