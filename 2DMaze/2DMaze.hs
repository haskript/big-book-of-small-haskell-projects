{- cabal:
ghc-options: -O2 -threaded
default-language: GHC2021
build-depends: base, vector, random, lens, ansi-terminal
-}
{-# LANGUAGE LambdaCase, ScopedTypeVariables #-}

module Main where

import System.IO
    ( FilePath
    , hSetBuffering
    , stdout
    , stdin
    , BufferMode(NoBuffering, LineBuffering)
    , hSetEcho
    , hGetChar
    , hFlush
    , hReady
    )
import System.Console.ANSI (clearScreen)
import System.Environment (getArgs)
import System.Exit (die, exitFailure, exitSuccess)
import Data.Vector (Vector)
import Data.Vector qualified as V

import Data.Function ((&))
import Data.Foldable (traverse_, foldl')
import Control.Exception (catch, evaluate, IOException)
import System.Random (uniformR, initStdGen)
import Data.Foldable (for_, traverse_)
import Control.Monad.IO.Class (liftIO)
import Control.Lens.Operators ((.~), (%~))
import Data.Vector.Lens (sliced)

import Control.Lens.Traversal (element)

-- | Representing Maze as a newtype, instead of a type synonym,
-- to provide for better type safety.
newtype Maze = MkMaze (Vector (Vector MazeTile)) deriving Show

-- | MazeTile datatype for bidirectional parsing.
data MazeTile
    = Clear
    | Wall
    | Exit
    | Start
    deriving (Show, Eq)

-- | Strict position tuple.
data Position = MkP !Int !Int deriving (Show, Eq)

-- | Relevant game data for looping.
data Game = MkGame
    { currentPosition :: !Position
    , exitPosition    :: !(Vector Position)
    , maze            :: !Maze
    } deriving Show

-- | Simple initialize, then if further actions are needed,
-- mazeProg will act on the returned Game data.
main :: IO ()
main = initialize >>= mazeProg

-- | Set higher performance print, then getArg
-- for considerArgs, and if considerArgs
-- doesn't dispatch early exit, initialize
-- stdin with NoBuffering and no echo.
-- 
-- Should be replaced with optparse-applicative later.
initialize :: IO Game
initialize = do
    hSetBuffering stdout LineBuffering
    
    (getArgs >>= considerArgs) <* do
        hSetBuffering stdin NoBuffering
	hSetEcho stdin False

-- | Maze prompt.
mazeHelp :: [String]
mazeHelp =
    [ "2DMaze - Maze file reader."
    , ""
    , "Usage: 2DMaze FILENAME"
    , "  Load FILENAME as a maze, and try to escape"
    , ""
    , "Options:"
    , "  --he:lp    Show help options"
    , "  -h        Same as above"
    ]

-- | Combination argument dispatcher and exception thrower.
-- This function will handle errors returned
-- from parseMaze, using custom exception functions.
--
-- If there is more than one start-position given,
-- it will randomly select one to enable play.
considerArgs :: [String] -> IO Game
considerArgs = \case
    []                                     ->
        exitWith "No maze file given."
    [helpFlag]
        | helpFlag `elem` ["--help", "-h"] ->
	help >> exitSuccess
    [filename]                             ->
        loadFile filename
    _                                      ->
        exitWith "Superfluous arguments given."
  where
    exitWith :: String -> IO a
    exitWith errorMessage = do
        traverse_ putStrLn ["",errorMessage,""]
	help
	exitFailure

    help :: IO ()
    help = traverse_ putStrLn mazeHelp

    loadFile :: FilePath -> IO Game
    loadFile filePath = do
        mazeFile <- readFile filePath `catch` (\(ex :: IOException) ->
	    simpleError
	        (filePath <> " does not exist or could not be loaded.") )
	
	case parseMaze mazeFile of
	    Nothing -> simpleError
	        $ filePath <> "was loaded, but could not be read."
	    Just ([], _, _) -> simpleError
	        $ "Starting position could not be found in " <> filePath <> "."
	    Just (_, [], _) -> simpleError
	        $ "End position could not be found in " <> filePath <> "."
	    Just (_,_,MkMaze maze) -- Nothing in the booleans should ever
	                           -- be triggered, but just in case.
	        | V.length maze == 0 -> simpleError
		$ "Maze in " <> filePath <> " parsed with zero rows."
		| any (== 0) (V.length <$> maze) -> simpleError
		$ "Maze in " <> filePath <> " parsed with zero columns."
	    Just (startingPositions, exitPositions, maze) -> do
	        let startings = length startingPositions
                (rnd, _) <- uniformR (0, startings - 1) <$> initStdGen

		pure $ MkGame
		    (startingPositions !! rnd) (V.fromList exitPositions) maze
	    _ -> simpleError "Unknown error."

    simpleError :: String -> IO a
    simpleError message = traverse putStrLn
            [ ""
	    , message
	    , ""
	    ]
	>>  exitFailure

type CurrentPosition = Position
type ExitPosition = Position

type ParseMazeAccum
    = Maybe
          ([Position], [Position], Int, Int, Int,
              ([MazeTile] -> [MazeTile]), [[MazeTile]] -> [[MazeTile]])

-- | Single traversal maze parser, returning general parsing errors
-- as Maybe, as well as empty start position lists and exit position
-- lists.
parseMaze :: String -> Maybe ([CurrentPosition], [ExitPosition], Maze)
parseMaze string = dListToMaze
    <$> foldr scanMaze endScan string (Just ([],[],0,0,0,id,id))
  where
    dListToMaze
        :: ( [CurrentPosition]
	   , [ExitPosition]
	   , Int
	   , Int
	   , Int
	   , [MazeTile] -> [MazeTile]
	   , [[MazeTile]] -> [[MazeTile]]
	   )
	-> ([CurrentPosition], [ExitPosition], Maze)
    dListToMaze (curpos, exitpos, maxCol, _, maxRow, acc, dList) =
        ( curpos, exitpos, MkMaze resultVec )
      where
        resultVec = expand . V.fromList <$> V.fromList sourceList

	expand vec = let diff = maxCol - V.length vec in
	    if diff <= 0
	  then vec
	  else vec <> V.replicate diff Clear

	sourceList = dList []

    scanMaze
        :: Char
	-> (ParseMazeAccum -> ParseMazeAccum)
	-> (ParseMazeAccum -> ParseMazeAccum)
    scanMaze char cont !Nothing = Nothing
    scanMaze char cont
        !args@( Just (startPos, exitPos, maxCol, currentCol, currentRow, acc
	      , dList) )
        = case char of
        ' ' -> cont $ Just
	    ( startPos, exitPos, newMaxCol, currentCol + 1, currentRow
	    , acc . (Clear:), dList)
	'#' -> cont $ Just
	    ( startPos, exitPos, newMaxCol, currentCol + 1, currentRow
	    , acc . (Wall:), dList)
	'S' -> cont $ Just
	    ( MkP currentCol currentRow :startPos, exitPos, newMaxCol
	    , currentCol + 1, currentRow, acc . (Start:), dList)
	'E' -> cont $ Just
	    ( startPos, MkP currentCol currentRow : exitPos, newMaxCol
	    , currentCol + 1, currentRow, acc . (Exit:), dList)
	'\n' -> cont $ Just
	    ( startPos, exitPos, maxCol, 0, currentRow + 1, id
	    , dList . (acc []:) )       
        '\r' -> cont args
	_ -> Nothing
      where
        newMaxCol = if currentCol == maxCol
	    then maxCol + 1
	    else maxCol
    
    endScan :: ParseMazeAccum -> ParseMazeAccum
    endScan Nothing = Nothing
    endScan (Just (startpos, endpos, maxCol, curCol, maxRow, acc, dList)) =
        Just updated
      where
        updated | null $ acc [] = (startpos, endpos, maxCol, curCol,
	    maxRow + 1, id, dList)
	        | otherwise = 
	    (startpos, endpos, maxCol,
	    curCol, maxRow + 1, acc, dList . (acc []:))

-- | Main loop of the game.
mazeProg :: Game -> IO ()
mazeProg game@( MkGame (MkP col row) exits ( MkMaze maze ) ) = do
    clearScreen

    helpNotice
    putMaze
    putStrLn ""

    getInput
  where
    helpNotice :: IO ()
    helpNotice = traverse_ putStrLn
        [ ""
	, "Press arrow keys to move through the maze."
	, ""
	, "Lowercase q exits."
	, ""
	]

    putMaze :: IO ()
    putMaze = for_ displayMaze $ \line -> do
        for_ line $ \mazeTile -> putStr [mazeTile]
        putStrLn ""

    displayMaze :: Vector (Vector Char)
    displayMaze = fmap toDisplay <$> maze
        & element row . element col .~ '@'

    toDisplay :: MazeTile -> Char
    toDisplay = \case
        Clear -> '.'
	Wall -> '#'
	Exit -> 'E'
	Start -> 'S'
    
    getInput :: IO ()
    getInput = do
        input <- getPress
	case input of
	       "q" -> pure ()
	       "\ESC[A" -> checkCoords $ MkP col (row - 1) --UP
	       "\ESC[B" -> checkCoords $ MkP col (row + 1) --DOWN
	       "\ESC[C" -> checkCoords $ MkP (col + 1) row --RIGHT
	       "\ESC[D" -> checkCoords $ MkP (col - 1) row --LEFT
	       _ -> getInput

    getPress :: IO String
    getPress = do
        key <- getChar
	continue <- hReady stdin
	if not continue
	    then pure [key]
	    else (key:) <$> getPress

    checkCoords :: Position -> IO ()
    checkCoords newCoords@(MkP col' row')
	| outOfBounds || hitWall = getInput
        | inExit = victory
	| otherwise = mazeProg game {currentPosition = newCoords}
      where
	outOfBounds = col' < 0 || col' >= V.length maze
	    || row' < 0 || row' >= V.length (maze V.! 0)

        hitWall = maze V.! row' V.! col' == Wall
	inExit = MkP col' row' `V.elem` exits

victory :: IO ()
victory = clearScreen >> traverse_ putStrLn
    [ "*****************"
    , ""
    , "You are a winner!"
    , ""
    , "*****************"
    ]
