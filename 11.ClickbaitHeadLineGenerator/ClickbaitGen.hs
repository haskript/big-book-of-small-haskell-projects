{- cabal:
default-language: GHC2021
build-depends: base, random, vector, text, text-show, transformers
-}
{-# LANGUAGE OverloadedLists, OverloadedStrings, TypeApplications #-}
module Main where

import Data.Vector (Vector)
import Data.Vector qualified as V
import System.Random (initStdGen, randomR, StdGen)
import Data.String (IsString, fromString)
import TextShow (showt)
import Text.Read (readMaybe)
import Data.Text (Text, toLower, pack)
import Data.Text.IO qualified as TIO
import Control.Monad (join, replicateM_)
import Data.Functor ((<&>))
import Data.Foldable (traverse_)
import System.IO (hFlush, stdout)
import Control.Monad.Trans.State.Strict
    ( State
    , evalState
    , get
    , put
    , modify' )
import Control.Arrow (first)

type RndState = State StdGen

rndStateToIO :: State StdGen a -> IO a
rndStateToIO term = evalState term <$> initStdGen

instance IsString (RndState Text) where
    fromString = pure . pack

instance Semigroup a => Semigroup (RndState a) where
    a <> b = liftA2 (<>) a b

objectPronouns, possessivePronouns, personalPronouns,
    states, nouns, places, when,
    websites :: Vector Text
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

funcBatch :: Vector (RndState Text)
funcBatch@[ noun, aWhen, objectPronoun, state, possessivePronoun, place
    , personalPronoun, website ] = select <$> [ nouns, when, objectPronouns,
    states, possessivePronouns, places, personalPronouns, websites ]

intro :: Vector Text
intro =
    [ "Clickbait Headline Generator"
    , "By Liam Zhu liam.zhu@protonmail.com, adapted from"
    , "original Python version by Al Sweigart al@inventwithppython.com"
    , ""
    , "Our website needs to trick people into looking at ads!"
    ]

headlines :: Vector (RndState Text)
headlines =
    [ "Are Millenials Killing the " <> noun <> " Industry?" 
    , "Without This " <> noun <> ", " <> noun <> "s Could Kill You " <> aWhen
    , "Big Companies Hate " <> objectPronoun <> "! See How This " <> state
          <> " " <> noun <> " Invented a Cheaper " <> noun
    , "You Won't Believe What This " <> state <> " " <> noun <> " Found in "
          <> possessivePronoun <> " " <> place
    , "What " <> noun <> "s Don't Want You To Know About " <> noun <> "s"
    , (showt <$> selectNumber 7 15 ) <> " Gift Ideas to Give Your " <> noun
          <> " From " <> state
    , do initialNumber <- selectNumber 3 19
         pure (showt initialNumber) <> " Reasons Why " <> noun
             <> " Are More Interesting Than You Think (Number "
             <> (showt <$> selectNumber 1 initialNumber)
	     <> " Will Surprise You!)"
    , do num <- selectNumber 0 2
         "This " <> state <> " " <> noun <> " Didn't Think Robots Would Take "
	     <> (pure $ possessivePronouns V.! num) <> " Job. "
	     <> (pure $ personalPronouns V.! num) <> " "
	     <> if num == 2 then "Were" else "Was" <> " Wrong"
    ]

select :: Vector a -> RndState a
select vec = do
    gen <- get
    let (output, newGen) = first (vec V.!) $ randomR (0, V.length vec - 1) gen
    put newGen
    pure output

selectNumber :: Int -> Int -> RndState Int
selectNumber lower upper = do
    gen <- get
    let (output, newGen) = randomR (lower, upper) gen
    put newGen
    pure output

main :: IO ()
main = traverse_ TIO.putStrLn intro >> inputLoop

inputLoop :: IO ()
inputLoop = do
    TIO.putStrLn "Enter the number of clickbait headlines to generate:"
    number <- TIO.putStr "> " >> hFlush stdout >> getLine
    case readMaybe number :: Maybe Int of
        Nothing -> TIO.putStrLn "Please enter a number." >> inputLoop
	Just count -> do
            replicateM_ count
	        $ TIO.putStrLn =<< rndStateToIO ( join $ select headlines )
	    TIO.putStrLn ""
	    TIO.putStrLn =<< rndStateToIO ("Post these to our " <> website
	        <> " " <> fmap toLower aWhen <> " or you're fired!")
