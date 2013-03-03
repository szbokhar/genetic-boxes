module Program.State
    ( State(..), DisplayMode(..), AlgorithmPhases(..), initializeState
    , autoTimestep, defaultCircleCount
    , increasePopulation, rankPopulation, selectPopulation, matePopulation
    , pairup )
where

import Control.Applicative  ( (<$>) )
import Control.Monad        ( forM )
import Data.IORef           ( IORef, newIORef )
import Data.List            ( sort )
import Graphics.Rendering.OpenGL ( GLsizei, ($=), get )

import Data.BallBox ( BallBox(boxId), mateBoxes, randomBox )

-- |Controls the speed of transition from phases of automate mode
autoTimestep :: Int
autoTimestep = 5

-- |Controls the speed of transition from phases of automate mode
defaultCircleCount :: Int
defaultCircleCount = 15

-- |State type that contains all global information about the program.
--  Typically passed to all callback functions so that they may
--  have a means to communicate.
data State =
     State  { width     :: IORef GLsizei        -- Width of the window
            , height    :: IORef GLsizei        -- Height of the widow
            , close     :: IORef Bool           -- Flag to quit program
            , prompt    :: IORef Bool           -- Flag to print prompt tick
            , drawList  :: IORef [IO ()]        -- List of draws to execute
            , drawMode  :: IORef DisplayMode    -- How to set up draw
            , boxes     :: IORef [BallBox]      -- Boxes in population
            , nextBoxId :: IORef Int }          -- ID tp give next box generated

-- |Data type to keep trak of the display mode of the program
data DisplayMode = Population | Mating | Automate Int Int AlgorithmPhases
  deriving (Show, Read, Eq)

-- |Data type to keep track of the phase of the automate drun of the algorithm
data AlgorithmPhases = Display | Sort | Select | Mate
  deriving (Show, Read, Eq, Enum)

-- |contains an initialized State value
initializeState :: IO State
initializeState = do
    w <- newIORef 0
    h <- newIORef 0
    c <- newIORef False
    p <- newIORef True
    d <- newIORef []
    mode <- newIORef Population
    b <- newIORef []
    bid <- newIORef 0
    return (State w h c p d mode b bid)



-- Functions to update the state of the population
increasePopulation :: State -> Int -> IO ()
increasePopulation state n = do
    -- Update box id counter
    bid <- get $ nextBoxId state
    nextBoxId state $= bid + n
    -- Generate n new boxes
    newBoxes <- forM [1..n] $ (\i -> do
        randomBox (bid+i) (100,100) defaultCircleCount)
    -- Update the state
    xs <- get $ boxes state
    boxes state $= xs++newBoxes

-- |Sorts the population
rankPopulation :: State -> IO ()
rankPopulation state = do
    xs <- get $ boxes state
    boxes state $= sort xs

-- |Keeps the first n of the population
selectPopulation :: State -> Int -> IO ()
selectPopulation state n = do
    xs <- get $ boxes state
    boxes state $= take n xs

-- |Pairs and mates the existing members of the population
matePopulation :: State -> IO ()
matePopulation state = do
    -- Get relavant data from state
    xs <- get $ boxes state
    startId <- get $ nextBoxId state
    -- Generate new children
    newChildren <- concat <$> (mapM (uncurry $ mateBoxes 2) $ pairup xs )
    let idChildren = zipWith (\bb n -> bb { boxId = n }) newChildren [startId..]
    -- Update state
    boxes state $= (xs ++ idChildren)
    drawMode state $= Mating



-- Utility functions and constants
pairup :: [a] -> [(a,a)]
pairup []       = []
pairup [_]      = []
pairup (x:y:xs) = (x,y) : pairup xs
