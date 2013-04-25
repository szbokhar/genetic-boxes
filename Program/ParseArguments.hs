module Program.ParseArguments where

import Util.ToInt

data ProgramOptions =
     ProgramOptions { optHelp               :: Bool
                    , optTimestep           :: Int
                    , optSize               :: (Int, Int)
                    , optInitCircleCount    :: Int
                    , optNoGui              :: Bool
                    , optOutputFile         :: Maybe String
                    , optInputFile          :: Maybe String
                    , parseError            :: Maybe String }
  deriving (Show, Read, Eq)

defaultOptions :: ProgramOptions
defaultOptions =
    ProgramOptions { optHelp = False
                   , optTimestep = 2
                   , optSize = (800, 600)
                   , optInitCircleCount = 15
                   , optNoGui = False
                   , optOutputFile = Nothing
                   , optInputFile = Nothing
                   , parseError = Nothing }

parseArguments :: ProgramOptions -> [String] -> ProgramOptions
parseArguments opts [] = opts
parseArguments opts ("-h":xs) = parseArguments (opts { optHelp = True }) xs

parseArguments opts ("-t":sNum:xs) =
    parseArguments (case num of
        Nothing -> opts { parseError = msg }
        Just n  -> opts { optTimestep = n }
    ) xs
  where num = int' sNum
        msg = Just "Must supply -t option with a valid integer"

parseArguments opts ("-d":sWidth:sHeight:xs) =
    parseArguments (case (width, height) of
        (Nothing,_)         -> opts { parseError = msg }
        (_,Nothing)         -> opts { parseError = msg }
        (Just w, Just h)    -> opts { optSize = (w,h) }
    ) xs
  where
    width = int' sWidth
    height = int' sHeight
    msg = Just "Must supply -d option with two valid integers"

parseArguments opts (x:_) =
    opts { parseError = Just $ "Unrecognized option '" ++ x ++ "'" }
