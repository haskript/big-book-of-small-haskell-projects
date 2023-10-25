{- cabal:
ghc-options: -O2
default-language: GHC2021
build-depends: base, random, containers, transformers
-}
{-# LANGUAGE ViewPatterns #-}
import Control.Concurrent
    ( forkIO
    , killThread
    , threadDelay
    )
import System.IO
    ( hSetBuffering
    , stdin
    , stdout
    , BufferMode (NoBuffering, LineBuffering)
    , hSetEcho
    )
import System.Random (initStdGen, randomR, StdGen)
import Control.Monad.Trans.State (StateT, evalStateT, put, get)

-- import Data.Sequence

type Cave = (Int, Int, Int, Int, StdGen)

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout LineBuffering
    hSetEcho stdin False

    seed <- initStdGen

    thread <- forkIO . deepCaveScreen $ defaults seed
    getChar
    killThread thread
  where
    defaults = ( 10, 5, 40, 3_125,)

deepCaveScreen :: Cave -> IO ()
deepCaveScreen cave@(gapStart, gapLength, totalLength, delay, gen) = do
    putStrLn $ makeCaveTile cave

    threadDelay delay

    deepCaveScreen $ updateCave cave

makeCaveTile :: Cave -> String
makeCaveTile (gapStart, gapLength, totalLength, _, gen) =
       replicate gapStart '\x00AE'
    <> replicate gapLength '^'
    <> replicate (totalLength - gapLength - gapStart) '\x00AE'

updateCave :: Cave -> Cave
updateCave (gapStart, gapLength, totalLength, delay, gen) =
    (newStart, newLength, totalLength, delay, retGen)
  where
    (newStart, newLength) = if reasonable
        then (candStart, candLength)
	else (gapStart, gapLength)

    reasonable = candStart > 0
        && candStart + candLength < totalLength
	&& 3 < candLength

    ((+ gapStart) -> candStart,
      ( (+ gapLength) -> candLength , retGen))
         = randomR (-1,1) <$> randomR (-1,1) gen
