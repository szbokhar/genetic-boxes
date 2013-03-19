module Program.ParseArguments where

import GL.Aliases   ( readInt )

data ProgramOptions =
     ProgramOptions { optHelp               :: Bool
                    , optTimestep           :: Int
                    , optInitCircleCount    :: Int
                    , optNoGui              :: Bool
                    , optOutputFile         :: Maybe String
                    , optInputFile          :: Maybe String
                    , parseError            :: Maybe String }
  deriving (Show, Read, Eq)

defaultOptions =
    ProgramOptions { optHelp = False
                   , optTimestep = 2
                   , optInitCircleCount = 15
                   , optNoGui = False
                   , optOutputFile = Nothing
                   , optInputFile = Nothing
                   , parseError = Nothing }

parseArguments opts [] = opts
parseArguments opts ("-h":xs) = parseArguments (opts { optHelp = True }) xs
parseArguments opts ("-t":sNum:xs) = parseArguments (case num of
    Nothing -> opts { parseError = msg }
    Just n  -> opts { optTimestep = n }) xs
  where num = readInt sNum
        msg = Just "Must supply -t option with a valid integer"


