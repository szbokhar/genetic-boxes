module Program.State where

import Control.Monad    ( forM )
import Graphics.Gloss
import System.Exit      ( exitSuccess )

import Data.BallBox
import Program.ParseArguments

-- |Controls the speed of transition from phases of automate mode
autoTimestep :: Int
autoTimestep = 1

-- |Controls the speed of transition from phases of automate mode
defaultCircleCount :: Int
defaultCircleCount = 15

-- |State type that contains all global information about the program.
--  Typically passed to all callback functions so that they may
--  have a means to communicate.
data State =
     State  { viewSize  :: (Int, Int)     -- Width of the window
            , close     :: Bool           -- Flag to quit program
            , prompt    :: Bool           -- Flag to print prompt tick

            , drawMode  :: DisplayMode    -- How to set up draw
            , boxes     :: [BallBox]      -- Boxes in population
            , nextBoxId :: Int            -- ID tp give next box generated
            , gens      :: Int }          -- Number of generations
  deriving Show

-- |Data type to keep trak of the display mode of the program
data DisplayMode = Population
                 | Mating
                 | Automate Int Int AlgorithmPhases
  deriving (Show, Read, Eq)

-- |Data type to keep track of the phase of the automate drun of the algorithm
data AlgorithmPhases = Display
                     | Sort
                     | Select
                     | Shuffle
                     | Mate
  deriving (Show, Read, Eq, Enum)

-- |contains an initialized State value
initializeState :: ProgramOptions -> State
initializeState opts =
    State (optSize opts) False True Population [] 0 0

timeUpdate :: Float -> State -> IO State
timeUpdate time st
    | close st      = exitSuccess
    | otherwise     = return st

drawState state = return Blank

-- Functions to update the state of the population
increasePopulation :: State -> Int -> IO State
increasePopulation st n = do
    -- Update box id counter
    let bid = nextBoxId st
    -- Generate n new boxes
    newBoxes <- forM [1..n]
                     (\i -> randomBox (bid+i) (100,100) defaultCircleCount )
    -- Update the state
    return st { boxes = boxes st ++ newBoxes
              , nextBoxId = bid + n }

{-- |Sorts the population
rankPopulation :: State -> IO ()
rankPopulation state = do
    xs <- get $ boxes state
    boxes state $= sort xs

-- |Keeps the first n of the population
selectPopulation :: State -> Int -> IO ()
selectPopulation state n = do
    xs <- get $ boxes state
    boxes state $= take n xs

-- |Do stuff before mating
preMate :: State -> IO ()
preMate state = do
    bs <- get $ boxes state
    shuffled <- shuffleM bs
    boxes state $= shuffled

-- |Pairs and mates the existing members of the population
matePopulation :: State -> IO ()
matePopulation state = do
    -- Get relavant data from state
    xs <- get $ boxes state
    startId <- get $ nextBoxId state
    -- Generate new children
    newChildren <- concat <$> mapM (uncurry $ mateBoxes 2) (pairup xs)
    let idChildren = zipWith (\bb n -> bb { boxId = n }) newChildren [startId..]
    -- Update state
    boxes state $= (xs ++ idChildren)
    g <- get $ gens state
    gens state $= g+1

--}

-- Utility functions and constants
pairup :: [a] -> [(a,a)]
pairup []       = []
pairup [_]      = []
pairup (x:y:xs) = (x,y) : pairup xs
