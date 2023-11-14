{- cabal:
default-language: GHC2021
ghc-options: -threaded
build-depends: base
-}

module Main where

main = mapM_ putStrLn $ hexagons 12 17

hexagons xRepeat yRepeat = concat
      . replicate yRepeat
      $ concat
      . replicate xRepeat
    <$> [ "/ \\_", "\\_/ "]
