{- cabal:
default-language: GHC2021
build-depends: base, ansi-terminal, lens, vector
-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Data.Foldable (traverse_, sequenceA_, fold)
import System.Console.ANSI
    ( clearScreen
    , setSGR
    , SGR (..)
    , ConsoleIntensity (..)
    , Color (..)
    , ColorIntensity (..)
    , ConsoleLayer (..)
    )
import System.IO
    ( hReady
    , stdin
    , stdout
    , hSetEcho
    , hSetBuffering
    , BufferMode (..)
    , hGetBuffering
    )

import Data.Bool (bool)
import Data.List (intersperse, uncons)

import Data.Function ((&))

import Control.Monad (replicateM)

import Control.Lens.Traversal
import Control.Lens.Operators ((%~))

import Data.Vector qualified as V
import Data.Vector (Vector)

data Hanoi
    = MkHanoi
    { firstPeg :: [DiskCount]
    , secondPeg :: [DiskCount]
    , thirdPeg :: [DiskCount]
    , selectState :: Maybe PegNumber
    , focusState :: PegNumber
    } deriving (Eq, Show)

data PegNumber
    = PegOne
    | PegTwo
    | PegThree
    deriving (Eq, Show, Enum)

data DiskCount
    = Zero
    | One
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    deriving (Eq, Show, Enum, Ord)

hanoiDefault :: Hanoi
hanoiDefault = MkHanoi defaultStack [] [] Nothing PegTwo
  where
    defaultStack = [ One .. Nine ]

main :: IO ()
main = initialize >> gameLoop hanoiDefault >> exit
  where
    exit = setSGR [Reset] >> putStrLn ""

    debugHanoi = hanoiDefault { thirdPeg = [ One .. Nine ] }

initialize :: IO ()
initialize = do
    setSGR defaultSGR
 
    hSetBuffering stdin NoBuffering

    hSetEcho stdin False

    hSetBuffering stdout LineBuffering

defaultSGR :: [SGR]
defaultSGR = 
    [ SetColor Foreground Vivid White
    , SetColor Background Dull Blue
    ]

gameLoop :: Hanoi -> IO ()
gameLoop hanoi
    | checkVictory = do
        clearScreen

	putHanoi hanoi

	mapM_ putStrLn
	    [ ""
	    , "You are a winner!"
	    , ""
	    , "Thanks for playing and write more Haskell."
	    , ""
	    , ""
	    , "******************************************"
	    , "kthxbye."
	    ]
    | otherwise    = do
        clearScreen

        showIntro
        putHanoi hanoi
        putStrLn ""
        showInstructions
    
        getPress >>= evaluateInput
  where
    checkVictory :: Bool
    checkVictory = thirdPeg hanoi == [One .. Nine]

    showIntro :: IO ()
    showIntro = mapM_ putStrLn intro

    showInstructions :: IO ()
    showInstructions = mapM_ putStrLn instructions

    intro :: [String]
    intro =
        [ "The Tower of Hanoi, adapted from Al Sweigart's original"
        , "by Liam Zhu liam.zhu@protonmail.com"
        , ""
        , "Move the tower of disks, one disk at a time, to another tower."
        , "Larger disks cannot rest on top of a smaller disk."
        , ""
        , "More info at https://en.wikipedia.org/wiki/Tower_of_Hanoi"
        , ""
        ]

    instructions :: [String]
    instructions =
        [ ""
	, "Press the left or right arrow key to move the cursor."
        , ""
        , "Press space to select a peg and, on the same peg,"
        , "unselect, or on a different peg, to attempt to move"
        , "the top disk from the selected peg to the new peg."
        , ""
	, "Press q to exit."
	, ""
        ]

    evaluateInput :: String -> IO ()
    evaluateInput = \case
        "q" -> pure ()
	"\ESC[D" -> leftPress
        "\ESC[C" -> rightPress
	" " -> select
	_ -> gameLoop hanoi
      where
        leftPress
	    | focusState hanoi == PegOne
	        = gameLoop $ hanoi {focusState = PegThree}
	    | otherwise = gameLoop
	        $ hanoi {focusState = pred $ focusState hanoi}
        rightPress
	    | focusState hanoi == PegThree
	        = gameLoop $ hanoi {focusState = PegOne}
	    | otherwise = gameLoop
	        $ hanoi {focusState = succ $ focusState hanoi}
	select = case selectState hanoi of
	    Nothing -> gameLoop
	        $ hanoi {selectState = Just $ focusState hanoi}
	    Just selectedPeg
	        | selectedPeg == focusState hanoi -> unselect
		| otherwise -> checkMove selectedPeg

    unselect :: IO ()
    unselect = gameLoop $ hanoi { selectState = Nothing }

    noSelect :: IO ()
    noSelect = gameLoop hanoi

    checkMove :: PegNumber -> IO ()
    checkMove selectedPeg
        | null (getPeg selectedPeg hanoi)
	  || getPeg selectedPeg hanoi >= getPeg (focusState hanoi) hanoi
	  && (not . null $ getPeg (focusState hanoi) hanoi)
	    = noSelect
	| otherwise = gameLoop
	    $ (moveDisk selectedPeg $ focusState hanoi) { selectState = Nothing }

    getPeg :: PegNumber -> Hanoi -> [DiskCount]
    getPeg = \case
        PegOne -> firstPeg
	PegTwo -> secondPeg
	PegThree -> thirdPeg

    moveDisk :: PegNumber -> PegNumber -> Hanoi
    moveDisk source target =
	case uncons $ getPeg source hanoi of
	    Nothing -> hanoi
	    Just (newDisk, newSource) ->

	        let newTarget = newDisk : getPeg target hanoi in
	    
	        case (source, target) of
	            (PegOne, PegTwo) -> hanoi
	                { firstPeg = newSource, secondPeg = newTarget }
	            (PegOne, PegThree) -> hanoi
	                { firstPeg = newSource, thirdPeg = newTarget }
	            (PegTwo, PegOne) -> hanoi
	                { secondPeg = newSource, firstPeg = newTarget }
	            (PegTwo, PegThree) -> hanoi
	                { secondPeg = newSource, thirdPeg = newTarget }
	            (PegThree, PegOne) -> hanoi
	                { thirdPeg = newSource, firstPeg = newTarget }
	            (PegThree, PegTwo) -> hanoi
	                { thirdPeg = newSource, secondPeg = newTarget }

data ColoredChar = MkCC [SGR] !Char deriving (Eq, Show)

putColoredChar :: ColoredChar -> IO ()
putColoredChar (MkCC sgr char) = do
    (case sgr of
        [] -> pure ()
	u -> setSGR u)
    putChar char
    setSGR defaultSGR

putHanoi :: Hanoi -> IO ()
putHanoi = (traverse_ . traverse_) putColoredChar
    . intersperse [MkCC defaultSGR '\n']
    . mkHanoiSpec

mkHanoiSpec :: Hanoi -> [[ColoredChar]]
mkHanoiSpec hanoi = appendFocus . appendSelect
    $ composedGrids hanoi
  where
    appendFocus input = input & flip select tintGrid
        (case focusState hanoi of
	PegOne -> one
	PegTwo -> two
	PegThree -> three)

    appendSelect input = flip select (const newElem)
        (case selectState hanoi of
            Nothing -> none
	    Just PegOne -> one
	    Just PegTwo -> two
	    Just PegThree -> three) $ input

    newElem = MkCC [ SetColor Background Vivid Red
	           , SetColor Foreground Vivid Red] '*'

    select (boolCheck1, boolCheck2) action target =
          target
        & elements (== 0) . elements boolCheck1 %~ action
        & elements (== 10) . elements boolCheck1 %~ action
	& elements oneToNine . elements boolCheck2 %~ action
      where
        oneToNine int = int > 0 && int < 10

    none = (const False, const False)
    one@(edgeSelects1, middleSelects1) =
        (\u -> u >= 0 && u <= 21, \u -> u == 0 || u == 21)
    two@(edgeSelects2, middleSelects2) =
        (\u -> u >= 21 && u <= 42, \u -> u == 21 || u == 42)
    three@(edgeSelects3, middleSelects3) =
        (\u -> u >= 42 && u <= 63, \u -> u == 42 || u == 63)

    tintGrid u = if u == newElem
        then MkCC [ SetColor Background Vivid Yellow
	          , SetColor Background Vivid Yellow ] 'X'
	else MkCC [ SetColor Background Dull Green
	          , SetColor Background Dull Green ] '%'

composedGrids :: Hanoi -> [[ColoredChar]]
composedGrids hanoi = (plainLine : appendTags) ++ [plainLine]
  where
    plainLine = replicate 64 plainTile

    plainTile = MkCC [] ' '

    appendTags = foldr go cont firstImage secondImage thirdImage
      where
	go x cont (y:ys) (z:zs) =
          ( (plainTile : x) ++ (plainTile : y)
            ++ (plainTile : z) ++ [plainTile] ) :
            cont ys zs

	cont _ _ = []

    [firstImage, secondImage, thirdImage] =
        fmap (appendRemainder . fmap drawDisks)
        [firstPeg hanoi , secondPeg hanoi, thirdPeg hanoi]

    appendRemainder :: [[ColoredChar]] -> [[ColoredChar]]
    appendRemainder acc = replicate (9-length acc)
        ( MkCC [] <$> "         ||         " ) <> acc

    drawDisks :: DiskCount -> [ColoredChar]
    drawDisks n = fmap (MkCC []) $
        replicate (9 - number) ' ' ++ replicate number '@' ++ 
        '_' : show number
	++ replicate number '@' ++ replicate (9 - number) ' '
      where
        number = fromEnum n

getPress :: IO String
getPress = do
    key <- getChar
    hReady stdin
        >>= bool (pure [key]) (fmap (key:) getPress)
