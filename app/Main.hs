module Main where

import System.Random
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Simulate
import Data.Time
import Text.Read
import Control.Monad.Trans.Writer ( WriterT, tell, runWriterT )
import Control.Monad.IO.Class ( liftIO )

main :: IO ()
main = do
        writeLineLog "Bouncing ball initialized..."

        (ball, logs) <- runWriterT createBall
        appendLogs logs

        -- Start simulation
        appendLineLog "Starting simulation..."
        simulateIO (window "Bouncing Ball" (getBound ball)) black 60 ball drawBall (const updateBall)
    where
        getBound :: Ball -> Point
        getBound (_, _, _, _, _, bound, _) = bound

        appendLogs :: [String] -> IO ()
        appendLogs (x:xs) = appendLineLog x >> appendLogs xs
        appendLogs [] = return ()

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
                    let newColors = popThenPush colors
                    appendLineLog $ "Ball new color is " ++ show (head newColors)
                    return newColors
                | otherwise = return colors

createBall :: WriterT [String] IO Ball
createBall = do
    liftIO $ putStrLn "Input bound X (100 - 400, def: 300): "
    unclampBoundX <- liftIO $ safeGetLineFloat 300
    let boundX = clamp 100 400 unclampBoundX
    let boundXLog = "Bound X is " ++ show boundX
    liftIO $ putStrLn boundXLog
    tell [boundXLog]

    liftIO $ putStrLn "Input bound Y (100 - 400, def: 300): "
    unclampBoundY <- liftIO $ safeGetLineFloat 300
    let boundY = clamp 100 400 unclampBoundY
    let boundYLog = "Bound Y is " ++ show boundY
    liftIO $ putStrLn boundYLog
    tell [boundYLog]

    liftIO $ putStrLn "Input ball size (10 - 50, def: 25): "
    unclampBallRadius <- liftIO $ safeGetLineFloat 25
    let ballRadius = clamp 10 50 unclampBallRadius
    let ballRadiusLog = "Ball radius is " ++ show ballRadius
    liftIO $ putStrLn ballRadiusLog
    tell [ballRadiusLog]

    liftIO $ putStrLn "Input initial ball speed (50 - 100, def: 75): "
    unclampInitVelocity <- liftIO $ safeGetLineFloat 75
    let initVelocity = clamp 50 100 unclampInitVelocity
    let ballInitVelocity = (initVelocity, initVelocity)
    let ballInitVelocityLog = "Ball initial velocity is " ++ show initVelocity
    liftIO $ putStrLn ballInitVelocityLog
    tell [ballInitVelocityLog]

    liftIO $ putStrLn "Input velocity increment (0 - 100, def: 50): "
    unclampIncVelocity <- liftIO $ safeGetLineFloat 50
    let incVelocity = clamp 0 100 unclampIncVelocity
    let ballIncVelocity = (incVelocity, incVelocity)
    let incVelocityLog = "Ball incremental velocity is " ++ show incVelocity
    liftIO $ putStrLn incVelocityLog
    tell [incVelocityLog]

    liftIO $ putStrLn "Input maximal velocity (50 - 2000, def: 1000): "
    unclampMaxVelocity <- liftIO $ safeGetLineFloat 1000
    let maxVelocity = clamp 50 2000 unclampMaxVelocity
    let ballMaxVelocity = (maxVelocity, maxVelocity)
    let maxVelocityLog = "Ball maximal velocity is " ++ show maxVelocity
    liftIO $ putStrLn maxVelocityLog
    tell [maxVelocityLog]

    x <- getStdRandom (randomR (-boundX, boundX))
    y <- getStdRandom (randomR (-boundY, boundY))
    let randomInitPosLog = "Ball random position is (" ++ show x ++ ", " ++ show y ++ ")"
    liftIO $ putStrLn randomInitPosLog
    tell [randomInitPosLog]

    let bound = (boundX, boundY)
    let ball = ((x, y), ballRadius, ballInitVelocity, ballIncVelocity, ballMaxVelocity, bound, [white, red, green, blue])
    return ball

safeGetLineFloat :: Float -> IO Float
safeGetLineFloat defValue = do
    value <- getLine
    let maybeValue = readMaybe value :: Maybe Float
    case maybeValue of
        Nothing -> return defValue
        Just x -> return x

clamp :: (Ord a)
      => a
      -- ^ The minimum value
      -> a
      -- ^ The maximum value
      -> a
      -- ^ The value to clamp
      -> a
clamp mn mx val = max mn (min val mx)

popThenPush :: [a] -> [a]
popThenPush [] = []
popThenPush (x:xs) = xs ++ [x]

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
