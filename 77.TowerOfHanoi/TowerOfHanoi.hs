{- cabal:
default-language: GHC2021
build-depends: base, ansi-terminal
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
    )

import Data.Bool (bool)
import Data.List (intersperse)

data Hanoi
    = MkHanoi
    { firstPeg :: DiscCount
    , secondPeg :: DiscCount
    , thirdPeg :: DiscCount
    , selectState :: Maybe PegNumber
    , focusState :: PegNumber
    } deriving (Eq, Show)

data PegNumber
    = PegOne
    | PegTwo
    | PegThree
    deriving (Eq, Show)

data DiscCount
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
    deriving (Eq, Show)

hanoiDefault :: Hanoi
hanoiDefault = MkHanoi Nine Zero Zero Nothing PegTwo

main :: IO ()
main = initialize >> gameLoop hanoiDefault >> exit
  where
    exit = setSGR [Reset]

initialize :: IO ()
initialize = do
    setSGR defaultSGR

    mapM_ (flip hSetBuffering NoBuffering) [stdin, stdout]

    hSetEcho stdin False

defaultSGR :: [SGR]
defaultSGR = 
    [ SetColor Foreground Vivid White
    , SetColor Background Dull Blue
    ]

gameLoop :: Hanoi -> IO ()
gameLoop hanoi = do
    clearScreen

    showIntro
    putHanoi hanoi
    showInstructions
    
    getPress >>= evaluateInput
  where
    showIntro = mapM_ putStrLn intro
    showInstructions = mapM_ putStrLn instructions

    evaluateInput = undefined

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

    instructions =
        [ "Press the left or right arrow key to move the cursor."
        , ""
        , "Press space to select a peg and, on the same peg,"
        , "unselect, or on a different peg, to attempt to move"
        , "the top disk from the selected peg to the new peg."
        , ""
        ]

data ColoredChar = MkCC [SGR] !Char

putColoredChar :: ColoredChar -> IO ()
putColoredChar (MkCC sgr char) = do
    setSGR sgr
    putChar char
    setSGR defaultSGR

putHanoi :: Hanoi -> IO ()
putHanoi =  (traverse_ . traverse_) putColoredChar
    . intersperse [MkCC [] '\n']
    . mkHanoiSpec

mkHanoiSpec :: Hanoi -> [[ColoredChar]]
mkHanoiSpec hanoi = appendFocus $ appendSelect composedGrids
  where
    appendFocus input = case focusState hanoi of
	PegOne -> undefined
	PegTwo -> undefined
	PegThree -> undefined

    appendSelect input = case selectState hanoi of
        Nothing -> input
	Just PegOne -> undefined
	Just PegTwo -> undefined
	Just PegThree -> undefined

    composedGrids = undefined

getPress :: IO String
getPress = do
    key <- getChar
    hReady stdin
        >>= bool (pure [key]) (fmap (key:) getPress)
