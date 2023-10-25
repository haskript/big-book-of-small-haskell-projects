{- cabal:
default-language: GHC2021
build-depends: base, random, transformers
-}
module Main where

import System.Random (initStdGen, randomR, StdGen)
import Text.Read (readMaybe)
import Control.Monad (join, replicateM_)
import System.IO (hFlush, stdout)
import Data.Char (toLower)
import Data.Foldable (fold)
import Control.Monad.Trans.State.Strict (State, evalState, get, put)

type RndState = State StdGen

rndStateToIO :: RndState a -> IO a
rndStateToIO rnd = evalState rnd <$> initStdGen

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

selectors :: [RndState String]
selectors@[ objectPronoun, possessivePronoun, personalPronoun, state
          , noun, place, aWhen, website ] = select <$> randomWords

intro :: [String]
intro = [ "Clickbait Headline Generator"
        , "By Liam Zhu liam.zhu@protonmail.com, adapted from"
        , "original Python version by Al Sweigart al@inventwithppython.com"
        , ""
        , "Our website needs to trick people into looking at ads!" ]

headlines :: [RndState String]
headlines = fmap fold <$>
    [ (\noun -> ["Are Millenials Killing the ", noun, " Industry?"])
        <$> noun
    , (\noun1 noun2 when -> ["Without This ", noun1, ", ", noun2
      , "s Could Kill You ", when] ) <$> noun <*> noun <*> aWhen
    , (\objectPronoun state noun1 noun2 -> ["Big Companies Hate "
      , objectPronoun, "! See How This ", state, " ", noun1
      , " Invented a Cheaper ", noun2 ] ) <$> objectPronoun <*> state 
        <*> noun <*> noun
    , (\state noun possessivePronoun place -> ["You Won't Believe What This "
      , state, " ", noun, " Found in ", possessivePronoun, " ", place])
        <$> state <*> noun <*> possessivePronoun <*> place
    , (\noun1 noun2 -> ["What ", noun1, "s Don't Want You To Know About "
      , noun2, "s"]) <$> noun <*> noun
    , (\number noun state -> [show number, " Gift Ideas to Give Your ", noun
      , " From ", state] ) <$> selectNumber 7 15 <*> noun <*> state
    , do reasonNum <- selectNumber 3 19
         (\noun num2 -> [ show reasonNum, " Reasons Why ", noun
             , " Are More Interesting Than You Think (Number "
             , show num2, " Will Surprise You!)" ] )
	     <$> noun <*> selectNumber 1 reasonNum
    , do num <- selectNumber 0 2
         (\state noun -> ["This ", state, " ", noun
             , " Didn't Think Robots Would Take "
             , possessivePronouns !! num, " Job. "
             , personalPronouns !! num, " "
             , if num == 2 then "Were" else "Was", " Wrong" ] )
	     <$> state <*> noun ]

select :: [a] -> RndState a
select ls =
    get >>= fmap (ls !!) . uncurry (<$) . fmap (put $!) . randomR (0, length ls - 1)

selectNumber :: Int -> Int -> RndState Int
selectNumber lower upper = get
    >>= uncurry (<$) . fmap (put $!) . randomR (lower, upper)

main :: IO ()
main = mapM_ putStrLn intro >> inputLoop

inputLoop :: IO ()
inputLoop = do
    putStrLn "Enter the number of clickbait headlines to generate:"
    number <- putStr "> " >> hFlush stdout >> getLine
    case readMaybe number :: Maybe Int of
        Nothing -> putStrLn "Please enter a number." >> inputLoop
        Just count -> do
            replicateM_ count
                $ putStrLn =<< ( rndStateToIO . join $ select headlines )
            putStrLn ""
            putStrLn =<< ( rndStateToIO $ fmap fold ( (\website when ->
	        [ "Post these to our ", website, " ", toLower <$> when, 
		" or you're fired!" ] ) <$> website <*> aWhen ) )
