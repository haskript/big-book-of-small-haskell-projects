{- cabal:
default-language: GHC2021
build-depends: base, random
-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Random (initStdGen, randomR, StdGen)
import Data.String (IsString, fromString)
import Text.Read (readMaybe)
import Control.Monad (join, replicateM_)
import System.IO (hFlush, stdout)
import Data.Char (toLower)

instance IsString (IO String) where
    fromString = pure

randomWords :: [[String]]
randomWords@[ objectPronouns, possessivePronouns, personalPronouns
            , states, nouns, places, when, websites ] =
  [ ["Her", "Him", "Them"], ["Her", "His", "Their"], ["She", "He", "They"]
  , [ "California", "Texas", "Florida", "New York", "Pennsylvania"
    , "Illinois", "Ohio", "Georgia", "North Carolina", "Michigan" ]
  , [ "Athlete", "Clown", "Shovel", "Paleo Diet", "Doctor", "Parent"
    , "Cat", "Dog", "Chicken", "Robot", "Video Game", "Avocado"
    , "Plastic Straw", "Serial Killer", "Telephone Psychic" ]
  , [ "House", "Attic", "Bank Deposit Box", "School", "Basement"
    , "Workplace", "Donut Shop", "Apocalypse Bunker" ]
  , [ "Soon", "This Year", "Later Today", "RIGHT NOW", "Next Week"]
  , [ "wobsite", "blag", "Facebuuk", "Googles", "Facesbook", "Tweedie"
    , "Pastagram"] ]

selectors :: [IO String]
selectors@[ objectPronoun, possessivePronoun, personalPronoun, state
          , noun, place, aWhen, website ] = select <$> randomWords

intro :: [String]
intro =
    [ "Clickbait Headline Generator"
    , "By Liam Zhu liam.zhu@protonmail.com, adapted from"
    , "original Python version by Al Sweigart al@inventwithppython.com"
    , ""
    , "Our website needs to trick people into looking at ads!"
    ]

headlines :: [IO String]
headlines =
    [ "Are Millenials Killing the " <> noun <> " Industry?"
    , "Without This " <> noun <> ", " <> noun <> "s Could Kill You " <> aWhen
    , "Big Companies Hate " <> objectPronoun <> "! See How This " <> state
      <> " " <>  noun <> " Invented a Cheaper " <> noun
    , "You Won't Believe What This " <> state <> " " <> noun <> " Found in "
      <> possessivePronoun <> " " <> place
    , "What " <> noun <> "s Don't Want You To Know About " <> noun <> "s"
    , (show <$> selectNumber 7 15) <> " Gift Ideas to Give Your " <> noun
      <> " From " <> state
    , do reasonNum <- selectNumber 3 19
         pure (show reasonNum) <> " Reasons Why " <> noun
             <> " Are More Interesting Than You Think (Number "
             <> (show <$> selectNumber 1 reasonNum) <> " Will Surprise You!)"
    , do num <- selectNumber 0 2
         "This " <> state <> " " <> noun <> " Didn't Think Robots Would Take "
             <> (pure $ possessivePronouns !! num) <> " Job. "
             <> (pure $ personalPronouns !! num) <> " "
	     <> if num == 2 then "Were" else "Was" <> " Wrong"
    ]

select :: [a] -> IO a
select ls = (ls !!) . fst . randomR (0, length ls - 1) <$> initStdGen

selectNumber :: Int -> Int -> IO Int
selectNumber lower upper = fst . randomR (lower, upper) <$> initStdGen

main :: IO ()
main = mapM_ putStrLn intro >> inputLoop

inputLoop :: IO ()
inputLoop = do
    putStrLn "Enter the number of clickbait headlines to generate:"
    number <- do putStr "> "
                 hFlush stdout
		 getLine

    case readMaybe number :: Maybe Int of
        Nothing -> putStrLn "Please enter a number." >> inputLoop
	Just count -> do
            replicateM_ count
	        $ putStrLn =<< ( join $ select headlines )
	    putStrLn ""
	    putStrLn =<< "Post these to our " <> website <> " "
	        <> (fmap . fmap) toLower aWhen <> " or you're fired!"
