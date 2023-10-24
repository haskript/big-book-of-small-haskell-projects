{- cabal:
default-language: GHC2021
build-depends: base, random, vector
-}
{-# LANGUAGE OverloadedLists, OverloadedStrings, TypeApplications #-}
module Main where

import Data.Vector (Vector)
import Data.Vector qualified as V
import System.Random (initStdGen, randomR)
import Data.String (IsString, fromString)
import Text.Read (readMaybe)
import Control.Monad (join, replicateM_)
import Data.Functor ((<&>))
import Data.Foldable (traverse_)
import System.IO (hFlush, stdout)
import Data.Char (toLower)

objectPronouns, possessivePronouns, personalPronouns,
    states, nouns, places, when,
    websites :: Vector String
objectPronouns = [ "Her", "Him", "Them" ]
possessivePronouns = [ "Her", "His", "Their" ]
personalPronouns = [ "She", "He", "They" ]
states = [ "California", "Texas", "Florida", "New York", "Pennsylvania"
         , "Illinois", "Ohio", "Georgia", "North Carolina", "Michigan" ]
nouns = [ "Athlete", "Clown", "Shovel", "Paleo Diet", "Doctor", "Parent"
        , "Cat", "Dog", "Chicken", "Robot", "Video Game", "Avocado"
        , "Plastic Straw", "Serial Killer", "Telephone Psychic" ]
places = [ "House", "Attic", "Bank Deposit Box", "School", "Basement"
         , "Workplace", "Donut Shop", "Apocalypse Bunker" ]
when = [ "Soon", "This Year", "Later Today", "RIGHT NOW", "Next Week"]
websites = [ "wobsite", "blag", "Facebuuk", "Googles", "Facesbook", "Tweedie"
           , "Pastagram"]

funcBatch :: Vector (IO String)
funcBatch@[ noun, aWhen, objectPronoun, state, possessivePronoun, place
    , personalPronoun, website ] = select <$> [ nouns, when, objectPronouns,
    states, possessivePronouns, places, personalPronouns, websites ]

intro :: Vector String
intro =
    [ "Clickbait Headline Generator"
    , "By Liam Zhu liam.zhu@protonmail.com, adapted from"
    , "original Python version by Al Sweigart al@inventwithppython.com"
    , ""
    , "Our website needs to trick people into looking at ads!"
    ]

instance IsString (IO String) where
    fromString = pure

headlines :: Vector (IO String)
headlines =
    [ "Are Millenials Killing the " <> noun <> " Industry?" 
    , "Without This " <> noun <> ", " <> noun <> "s Could Kill You " <> aWhen
    , "Big Companies Hate " <> objectPronoun <> "! See How This " <> state
          <> " " <> noun <> " Invented a Cheaper " <> noun
    , "You Won't Believe What This " <> state <> " " <> noun <> " Found in "
          <> possessivePronoun <> " " <> place
    , "What " <> noun <> "s Don't Want You To Know About " <> noun <> "s"
    , (show <$> selectNumber 7 15 ) <> " Gift Ideas to Give Your " <> noun
          <> " From " <> state
    , do initialNumber <- selectNumber 3 19
         pure (show initialNumber) <> " Reasons Why " <> noun
             <> " Are More Interesting Than You Think (Number "
             <> (show <$> selectNumber 1 initialNumber)
	     <> " Will Surprise You!)"
    , do num <- selectNumber 0 2
         "This " <> state <> " " <> noun <> " Didn't Think Robots Would Take "
	     <> (pure $ possessivePronouns V.! num) <> " Job. "
	     <> (pure $ personalPronouns V.! num) <> " "
	     <> if num == 2 then "Were" else "Was" <> " Wrong"
    ]

select :: Vector a -> IO a
select vec = initStdGen
    <&> (vec V.!) . fst . randomR (0, V.length vec - 1)

selectNumber :: Int -> Int -> IO Int
selectNumber lower upper = initStdGen
    <&> fst . randomR (lower, upper)

main :: IO ()
main = traverse_ putStrLn intro >> inputLoop

inputLoop :: IO ()
inputLoop = do
    putStrLn "Enter the number of clickbait headlines to generate:"
    number <- putStr "> " >> hFlush stdout >> getLine
    case readMaybe number :: Maybe Int of
        Nothing -> putStrLn "Please enter a number." >> inputLoop
	Just count -> do
            replicateM_ count
	        $ putStrLn =<< ( join $ select headlines )
	    putStrLn ""
	    putStrLn =<< "Post these to our " <> website <> " "
	        <> fmap (toLower <$>) aWhen <> " or you're fired!"
